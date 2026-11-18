import ledger

# close_journal_files() replaces only the journal; the commodity pool that
# ledger.commodities names at import survives it, so commodities created
# before the call and commodities parsed after it share one pool.

pool = ledger.commodities
pool.find_or_create("ZZZ")

ledger.close_journal_files()
ledger.session.close_journal_files()

print("ZZZ" in ledger.commodities)

journal = ledger.read_journal_from_string("""
2024/01/01 Opening
    Assets:Brokerage        10 QQQ
    Equity:Opening
""")

print("QQQ" in pool, "QQQ" in ledger.commodities)
print(pool.find("QQQ").symbol)
