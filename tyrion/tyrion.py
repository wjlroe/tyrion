#!/usr/bin/env python

import pika
import json
import os
import os.path
import subprocess
import shutil
import logging
import argparse

import config
from rabbitmq import RabbitMQ
from utils import *


class Tyrion:
    def __init__(self, config_path):
        logging.basicConfig(
            format='%(asctime)s %(name)s: %(levelname)s %(message)s',
            level=logging.DEBUG
        )

        self.config = config.Config()
        self.config.read_config(config_path)

        self.rabbitmq = RabbitMQ(self)

        self.rabbitmq.start()

    def deploy(self, push_data):
        repo_dir = mkdirs(self.config.rootdir, push_data['name'])
        shared_dir = mkdirs(repo_dir, "shared")
        releases_dir = mkdirs(repo_dir, "releases")
        sha_dir = os.path.join(releases_dir, push_data['commit'])
        current_loc = os.path.join(repo_dir, "current")
        clone_dir = os.path.join(shared_dir, "cached-copy")

        if not os.path.isdir(clone_dir):
            cmd = 'git clone %s %s' % (push_data['repo_url'], clone_dir)
            logging.info("Running: %s", cmd)
            subprocess.check_call(cmd, shell=True)

        reset_cmd = 'git fetch && git reset --hard %s' % push_data['commit']
        subprocess.check_call(reset_cmd,
                              shell=True, cwd=clone_dir)

        if os.path.isdir(sha_dir):
            shutil.rmtree(sha_dir)

        shutil.copytree(clone_dir, sha_dir)

        if os.path.exists(current_loc):
            os.unlink(current_loc)
        os.symlink(sha_dir, current_loc)

        # TODO: these need to be controlled some other way...
        for sym_dir in ['log', 'pids', 'cache']:
            path = mkdirs(shared_dir, sym_dir)
            link_loc = os.path.join(sha_dir, sym_dir)
            if os.path.isdir(link_loc):
                shutil.rmtree(link_loc)
            os.symlink(path, link_loc)

        # TODO: Run something in the repo itself, more generic
        reprocess = subprocess.Popen('bundle exec rake reprocess',
                                     shell=True, cwd=current_loc)
        logging.info('reprocess pid: %s', str(reprocess.pid))
        logging.info("deployed: %s", commit_sha)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('configfile', action='store')
    args = parser.parse_args()

    tyrion = Tyrion(args.configfile)
