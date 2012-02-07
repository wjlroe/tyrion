def receive(payload):
    return {
        'commit': payload['after'],
        'repo_url': http_to_ssh_github_url(payload['repository']['url']),
        'name': payload['repository']['name']
    }


def http_to_ssh_github_url(url):
    """
    Convert an http-based github repository URL to an ssh-based one
    """
    rm_http = url.replace('https://', '').replace('http://', '')
    ssh_url = rm_http.replace('github.com/', 'github.com:')
    return "git@%s.git" % ssh_url
