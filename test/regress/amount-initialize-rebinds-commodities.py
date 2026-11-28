import ledger

# Amount.shutdown() followed by Amount.initialize() is the one way Python
# code replaces the commodity pool.  ledger.commodities must name the new
# pool as soon as Amount.initialize() returns, without a later
# close_journal_files() call to rebind it.

old = ledger.commodities
old.find_or_create("ZZZ")

# While the pool is live, Amount.initialize() is a no-op and keeps it.
ledger.Amount.initialize()
print("ZZZ" in ledger.commodities)

ledger.Amount.shutdown()
ledger.Amount.initialize()

print("ZZZ" in ledger.commodities, "ZZZ" in old)

journal = ledger.read_journal_from_string("""
2024/01/01 Opening
    Assets:Brokerage        10 QQQ
    Equity:Opening
""")

print("QQQ" in ledger.commodities, "QQQ" in old)

ledger.close_journal_files()
print("ZZZ" in ledger.commodities, "QQQ" in ledger.commodities)
