
import sys
import os
server_path= '/home/jan/github.com/odoo14'
sys.setrecursionlimit(10000)

if os.path.isdir(server_path):
  try:
    sys.path.append(server_path)
    sys.path.append('pylibs')
    import odoo
    import odoo.tools.config
    from odoo.http import dispatch_rpc

    from datetime import datetime, timedelta
    from functools import partial
    from itertools import groupby

    from odoo import api, fields, models, SUPERUSER_ID, _
    from odoo.exceptions import AccessError, UserError, ValidationError
    from odoo.tools.misc import formatLang, get_lang
    from odoo.osv import expression
    from odoo.tools import float_is_zero, float_compare

    from werkzeug.urls import url_encode
    from odoo.tools import DEFAULT_SERVER_DATE_FORMAT, DEFAULT_SERVER_DATETIME_FORMAT
  except:
    print ("Could not import odoo14")
