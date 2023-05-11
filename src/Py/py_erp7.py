from __future__ import unicode_literals
import sys
import optparse
import os
import time
from mako.template import Template
from mako.runtime import Context
from StringIO import StringIO
#import datetime
import socket
import csv
import pprint
import numpy
import sys
import lxml.etree as etree

sys.setrecursionlimit(100000)
server_path='/home/jan/github.com/migrated_pjb70'
config_file='/home/jan/projects/server_pjbrefct.conf'
if os.path.isdir(server_path):   
    sys.path.append('pylibs')
    sys.path.append(server_path)
    import openerp
    import openerp.tools.config
    import openerp.service.web_services
    openerp.tools.config.parse_config(['--config=%s' % config_file])

    from openerp import netsvc
    from openerp.tools import DEFAULT_SERVER_DATE_FORMAT
    from openerp.tools import DEFAULT_SERVER_DATETIME_FORMAT    
    #import html_reports.controllers.main as r_main
    import galtyslib.openerplib as openerplib
    from openerp.osv import fields, osv
def print7_obj(o):
    print o
def get_connection7(db):
    pool, cr, uid = openerplib.get_connection(db)

    return {'pool':pool, 'cr':cr, 'uid':uid}
def split_sku(sku):
    sku.split('_')
    
def read_file_as_dict(pth):
    return eval(file(pth).read())
def pretty_xml(s):
    return etree.tostring(etree.fromstring(s),pretty_print=True) 
def resolve_xid(env, model, xid):
    try:
        (res_name, res_id) =env['pool'].get('ir.model.data').get_object_reference(env['cr'], env['uid'], model, xid)
        return res_id
    except:
        return (-1)
#ReturnTriple        
def return_tuple(code, err, msg):
    return (code, err, msg)
