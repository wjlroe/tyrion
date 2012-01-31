import pika


class RabbitMQ:
    def __init__(self, tyrion):
        self.tyrion = tyrion
        self.config = tyrion.config

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

        result = self.channel.queue_declare(exclusive=True)
        queue_name = result.method.queue

        self.channel.queue_bind(exchange=self.config.exchange,
                                queue=queue_name,
                                routing_key='github.push.*.*.*')

        self.channel.basic_consume(self.callback,
                                   queue=queue_name,
                                   no_ack=True)

    def callback(channel, method, properties, body):
        push_data = github.receive(json.loads(body))

        tyrion.deploy(push_data)

    def start(self):
        self.channel.start_consuming()
