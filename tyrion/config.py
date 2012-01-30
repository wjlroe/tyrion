import ConfigParser


class Config:
    def __init__(self):
        self.hostname = None
        self.username = None
        self.password = None
        self.exchange = None
        self.vhost = None
        self.rootdir = None

    def read_config(self, path):
        config = ConfigParser.RawConfigParser()
        config.read(path)
        self.hostname = config.get('amqp', 'hostname', '127.0.0.1')
        self.username = config.get('amqp', 'username', '')
        self.password = config.get('amqp', 'password', '')
        self.exchange = config.get('amqp', 'exchange', '')
        self.vhost = config.get('amqp', 'vhost', '/')
        self.rootdir = config.get('tyrion', 'rootdir', '/tmp/deploy')

