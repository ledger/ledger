def option_pyfirst(context):
    print "In --pyfirst (from %s)" % context

def option_pysecond(context, val):
    print "In --pysecond=%sh (from %s)" % (val, context)
