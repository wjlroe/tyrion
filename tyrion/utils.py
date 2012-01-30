import os.path
import os


def mkdirs(*args):
    """
    Join arguments into a pathname
    and create the directory if it doesn't already exist
    """
    mk_path = os.path.join(*args)
    if not os.path.exists(mk_path):
        os.makedirs(mk_path)
    return mk_path
