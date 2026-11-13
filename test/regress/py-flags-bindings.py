import ledger

# Every Python class that exposes the flags API (the flags property and
# has_flags/clear_flags/add_flags/drop_flags) must bind it through the flags
# base its C++ class really derives from, or each call raises ArgumentError.

session = ledger.Session()
journal = session.read_journal_from_string("""
2024/01/15 * Grocery Store
    Expenses:Food              $10.00
    Assets:Cash
""")

xact = next(iter(journal))
post = next(iter(xact.posts))
account = journal.find_account('Expenses:Food')
dollar = ledger.commodities['$']
lot = ledger.Amount('2 AAPL {$5}').commodity


def probe(label, obj, bit):
    try:
        before = obj.flags
        agree = all(obj.has_flags(1 << i) == bool(before & (1 << i))
                    for i in range(8))
        obj.add_flags(bit)
        added = obj.has_flags(bit)
        obj.drop_flags(bit)
        dropped = not obj.has_flags(bit)
        obj.flags = before | bit
        assigned = obj.flags == before | bit
        obj.clear_flags()
        cleared = obj.flags == 0
        obj.flags = before
        restored = obj.flags == before
        print('%s: int=%s agree=%s add=%s drop=%s set=%s clear=%s restore=%s'
              % (label, isinstance(before, int), agree, added, dropped,
                 assigned, cleared, restored))
    except Exception as err:
        print('%s: %s' % (label, type(err).__name__))


probe('Account', account, ledger.ACCOUNT_TEMP)
probe('AccountXData', account.xdata(), ledger.ACCOUNT_EXT_VISITED)
probe('Commodity', dollar, ledger.COMMODITY_NOMARKET)
probe('AnnotatedCommodity', lot, ledger.COMMODITY_NOMARKET)
probe('Annotation', ledger.Annotation(), 0x01)
probe('Transaction', xact, ledger.ITEM_TEMP)
probe('Posting', post, ledger.ITEM_TEMP)
probe('PostingXData', post.xdata(), ledger.POST_EXT_VISITED)
probe('SupportFlags8', ledger.SupportFlags8(0), 0x01)
probe('SupportFlags16', ledger.SupportFlags16(0), 0x100)
