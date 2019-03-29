# https://github.com/pazz/alot/wiki/Contrib-Hooks
import re

# Avoid sending e-mails without attachments
async def pre_envelope_send(ui, dbm, cmd):
    e = ui.current_buffer.envelope
    if re.match('.*[Aa]ttach', e.body, re.DOTALL) and\
       not e.attachments:
        msg = 'no attachments. send anyway?'
        if not (await ui.choice(msg, select='yes')) == 'yes':
            raise Exception()

# Ensure correct from addresses based on destination
transitions = [
    ('.*ginkgobioworks\.com.*',
     'Brad Chapman <bchapman@ginkgobioworks.com>')
    ]

addr_trans = []
for addr, fr in transitions:
    addr_trans.append((re.compile("(To|Cc): %s" % addr, re.MULTILINE),
                       "From: %s" % fr))

def pre_edit_translate(bodytext, ui, dbm):
    for addr, new_from in addr_trans:
        if addr.search(bodytext):
            return re.sub('^From: .*$', new_from, bodytext, flags=re.MULTILINE)
    return bodytext
