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
        configs = ConfigParser.RawConfigParser()
        configs.read(path)
        self.hostname = configs.get('amqp', 'hostname')
        self.username = configs.get('amqp', 'username')
        self.password = configs.get('amqp', 'password')
        self.exchange = configs.get('amqp', 'exchange')
        self.vhost = configs.get('amqp', 'vhost')
        self.rootdir = configs.get('tyrion', 'rootdir')
