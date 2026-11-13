import ledger

# Python 3 asks __bool__, never __nonzero__, for an object's truth value, so
# each binding's truth test must be registered under __bool__.

amount = ledger.Amount('$1')

print('Expr() %s' % bool(ledger.Expr()))
print("Expr('1') %s" % bool(ledger.Expr('1')))
print('Annotation() %s' % bool(ledger.Annotation()))
print('null_commodity %s' % bool(ledger.commodities.null_commodity))
print('$ commodity %s' % bool(amount.commodity))
print("Amount('0') %s" % bool(ledger.Amount('0')))
print("Amount('$1') %s" % bool(amount))
print('Balance() %s' % bool(ledger.Balance()))
print('Value(0) %s' % bool(ledger.Value(0)))
for name in ['Amount', 'Balance', 'Value', 'Expr', 'Annotation', 'Commodity']:
    cls = getattr(ledger, name)
    print('%s __bool__=%s __nonzero__=%s'
          % (name, hasattr(cls, '__bool__'), hasattr(cls, '__nonzero__')))
