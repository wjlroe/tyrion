import pika
import json
import os.path
import config
import github

class RabbitMQ:
    def __init__(self, config):
        self.config = config

        credentials = pika.PlainCredentials(self.config.username,
                                            self.config.password)
        connection = pika.BlockingConnection(pika.ConnectionParameters(
            host=self.config.hostname,
            virtual_host=self.config.vhost,
            credentials=credentials))
        self.channel = connection.channel()

        self.channel.exchange_declare(exchange=self.config.exchange,
                                      type='topic',
                                      durable=True)

    def callback(self, channel, method, properties, body):
        push_data = github.receive(json.loads(body))

        self.deploy_callback(push_data)

    def start(self, my_callback):
        self.deploy_callback = my_callback
        result = self.channel.queue_declare(exclusive=True)
        queue_name = result.method.queue

        self.channel.queue_bind(exchange=self.config.exchange,
                                queue=queue_name,
                                routing_key='github.push.*.*.*')

        self.channel.basic_consume(self.callback,
                                   queue=queue_name,
                                   no_ack=True)

        self.channel.start_consuming()

    def send_message(self):
        test_push = open(os.path.join(os.path.dirname(__file__),
                                      '..',
                                      'fixtures',
                                      'test-push-varys.json'), 'r').read()
        push = json.loads(test_push)
        routing_key = push["_meta"]["routing_key"]
        self.channel.basic_publish(exchange=self.config.exchange,
                                   routing_key=routing_key,
                                   body=json.dumps(push["payload"]))

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('configfile', action='store')
    args = parser.parse_args()

    config = config.Config()
    config.read_config(args.configfile)

    rabbitmq = RabbitMQ(config)
    rabbitmq.send_message()


