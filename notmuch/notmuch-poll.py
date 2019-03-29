#!/usr/bin/env python
"""Use notmuch python interface to poll for messages and assign initial tags.
"""
import subprocess
from contextlib import contextmanager

import notmuch

junkaddrs = ["ticketmaster.com", "education.vanguardinvestments.com",
             "vsathletics.com", "piperlimeg.delivery.net", "gap.delivery.net",
             "crashplan@kage.mgh.harvard.edu", "email.travelocity.com",
             "email.spiritairlines.com", "runnersworld.delivery.net",
             "shop.josbank.com", "store-news@amazon.com", "mybso@bso.org",
             "newsletter@socialbostonsports.com",
             "@ticketweb.com", "trendmicro.cs@digitalriver.com",
             "baevents@aol.com", "thewilburtheatre.com",
             "email@direct.livenation.com", "concertupdate@email.livenationent.com",
             "mideastclub.com", "olyent.com", "mail@marathonsports.com",
             "email.microcentermedia.com", "@LCPals.com", "@ipswitchmail.com",
             "BROADWAYACROSSAMERICA.COM", "cshpress@cshl.edu", "@health.ibemail.com",
             "@shcinc.org", "news@mindmeister.com", "update@stubhub-mail.com",
             "1800contacts.com", "@protherapysupplies.com", "@news1-active.com",
             "newsletter@greengranite.com", "donorservices@kexp.org",
             "warwickhotels@nndmail.com", "bxitems@bioinfo4u.org", "store-offers@amazon.com",
             "email@email.cbpresearch.com", "@clin-ed.com", "@healthtech.com", "@chacorporate.com",
             "@insightpharmareports.com", "@quora.com", "researchgate.net", "@zymoresearch.com",
             "@imausa.com", "@resources.sourceforge.com", "@getpocket.com", "@stemcell.com",
             "@biagenomics.net", "@activenewsletters.com", "@primbioresearchus.com", "@us-iran.org",
             "@aaisolutions.com", "@citationnews.org", "@usenix.org", "@sharevault.com",
             "@enews.tektronix.com", "@moderdrugsummit.com", "@pxlence.com", "@research.cyagenbio.com",
             "@worldpharmacongress.com", "@convergeditsummit.com", "@moderndrugsummit.com",
             "@sln.us.com", "@citusdata.com", "@selleckchemical.com", "@discoveryontarget.com",
             "@computer.org", "@eriksbikeshop.com", "@advaitabio.com", "@knowledgefoundation.com",
             "@lifescienceshome.com", "@itmodelbook.com", "@scienceboard.net", "@cdnasource.com",
             "@omicsgroup.com", "@mail.elsevier.com", "@bestservicemail.net", "@gtcinsight.com",
             "@barnettinternational.com", "@hirahongkong-tailer.com"]
deleteaddrs = ["sysadmin@curoverse.com", "git@public.curoverse.com", "@qq.com"]
junktitles = ["{SPAM"]
meaddrs = ["chapmanb@fastmail.com", "chapmanb@50mail.com", "bchapman@hsph.harvard.edu",
           "chapman@molbio.mgh.harvard.edu", "biovalidation@googlegroups.com",
           "brad.a.chapman@gmail.com", "bcbio-nextgen@noreply.github.com",
           "bcbio.variation@noreply.github.com", "brad.chapman@astrazeneca.com", "chapmanb@curoverse.com",
           "bchapman@ginkgobioworks.com"]
goodaddrs = ["anders@embl.de", "kim@chestnutfarm.org", "af-softball@googlegroups.com",
             "arklenna@gmail.com", "Lee.Ventola@astrazeneca.com", "info@eshcircusarts.com",
             "blue-parents@cambridgemontessori.org", "@cambridgemontessori.org"]
auto_tags = {"datomic": ["datomic@googlegroups.com"],
             "c2": ["c2-cljs@googlegroups.com"],
             "aleph": ["aleph-lib@googlegroups.com"],
             "pallet": ["pallet-clj@googlegroups.com"],
             }

def main():
    subprocess.check_call("notmuch new".split())
    db = notmuch.Database(mode=notmuch.Database.MODE.READ_WRITE)
    for msg in db.create_query("tag:new").search_messages():
        with msg_tx(msg) as msg:
            t = msg.get_header("to").lower()
            c = msg.get_header("cc").lower()
            f = msg.get_header("from").lower()
            s = msg.get_header("subject")
            add_to_inbox = True
            if is_from_me(f, meaddrs):
                add_to_inbox = False
                adjust_tags(msg, ["sent"], ["new"])
            elif is_delete(f, deleteaddrs):
                add_to_inbox = False
                adjust_tags(msg, ["deleted"])
            elif is_junk(f, [x.lower() for x in junkaddrs], s, junktitles):
                adjust_tags(msg, ["junk"])
            elif is_good(t, c, meaddrs, f, goodaddrs):
                adjust_tags(msg, ["good"])
            if add_to_inbox:
                adjust_tags(msg, ["inbox", "unread"], ["new"])
            for tag, emails in auto_tags.iteritems():
                if contains(t, emails):
                    adjust_tags(msg, [tag])
            #print t, f, s
            #print msg.get_tags()

def contains(base, want_list):
    for x in want_list:
        if base.find(x) >= 0:
            return True
    return False

def is_delete(from_str, addrs):
    return contains(from_str, addrs)

def is_from_me(from_str, addrs):
    return contains(from_str, addrs)

def is_junk(from_str, addrs, subject_str, subjects):
    return contains(from_str, addrs) or contains(subject_str, subjects)

def is_good(to_str, cc_str, addrs, from_str, from_addrs):
    return (contains(to_str, addrs) or
            contains(cc_str, addrs) or
            contains(from_str, from_addrs))

def adjust_tags(msg, to_add, to_remove=None):
    for t in to_add:
        msg.add_tag(t)
    if to_remove:
        for t in to_remove:
            msg.remove_tag(t)

@contextmanager
def msg_tx(msg):
    msg.freeze()
    try:
        yield msg
    finally:
        msg.thaw()
        msg.tags_to_maildir_flags()

if __name__ == "__main__":
    main()
