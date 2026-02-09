#!/usr/bin/env python3
"""Generate a 50K-posting benchmark journal for ledger performance testing."""

import random
from datetime import date, timedelta

random.seed(42)  # reproducible

accounts_expense = [
    "Expenses:Food:Groceries", "Expenses:Food:Restaurant",
    "Expenses:Housing:Rent", "Expenses:Housing:Utilities",
    "Expenses:Transport:Gas", "Expenses:Transport:Maintenance",
    "Expenses:Health:Insurance", "Expenses:Health:Pharmacy",
    "Expenses:Entertainment:Movies", "Expenses:Entertainment:Books",
    "Expenses:Clothing", "Expenses:Electronics",
    "Expenses:Travel:Hotels", "Expenses:Travel:Flights",
]

accounts_income = [
    "Income:Salary", "Income:Bonus", "Income:Interest",
    "Income:Dividends", "Income:Freelance",
]

accounts_asset = [
    "Assets:Checking", "Assets:Savings",
    "Assets:Investment:Brokerage", "Assets:Cash",
]

payees = [
    "Whole Foods", "Trader Joe's", "Amazon", "Shell Gas",
    "Netflix", "Starbucks", "Target", "Costco",
    "Home Depot", "Uber", "Delta Airlines", "Hilton",
    "Verizon", "Electric Company", "Water Dept",
    "Dr. Smith", "CVS Pharmacy", "Book Store",
    "Acme Corp", "Freelance Client", "Bank Interest",
]

commodities = ["AAPL", "GOOG", "MSFT", "VTI"]

start_date = date(2022, 1, 1)
posting_count = 0
target_postings = 50000

with open("benchmark/benchmark.dat", "w") as f:
    current_date = start_date

    # Add some price directives
    for i in range(100):
        d = start_date + timedelta(days=i * 10)
        for comm in commodities:
            base = {"AAPL": 150, "GOOG": 2800, "MSFT": 300, "VTI": 200}[comm]
            price = base + random.uniform(-20, 20)
            f.write(f"P {d} {comm} ${price:.2f}\n")
    f.write("\n")

    while posting_count < target_postings:
        current_date += timedelta(days=random.choice([0, 0, 0, 1]))

        r = random.random()

        if r < 0.6:
            # Simple 2-posting expense transaction
            payee = random.choice(payees)
            expense = random.choice(accounts_expense)
            source = random.choice(accounts_asset[:2])
            amount = round(random.uniform(5, 500), 2)
            f.write(f"{current_date} {payee}\n")
            f.write(f"    {expense}  ${amount:.2f}\n")
            f.write(f"    {source}\n\n")
            posting_count += 2

        elif r < 0.75:
            # 3-posting split transaction
            payee = random.choice(payees)
            exp1 = random.choice(accounts_expense)
            exp2 = random.choice(accounts_expense)
            source = random.choice(accounts_asset[:2])
            a1 = round(random.uniform(5, 200), 2)
            a2 = round(random.uniform(5, 200), 2)
            f.write(f"{current_date} {payee}\n")
            f.write(f"    {exp1}  ${a1:.2f}\n")
            f.write(f"    {exp2}  ${a2:.2f}\n")
            f.write(f"    {source}\n\n")
            posting_count += 3

        elif r < 0.85:
            # Income transaction
            income = random.choice(accounts_income)
            dest = random.choice(accounts_asset[:2])
            amount = round(random.uniform(500, 8000), 2)
            f.write(f"{current_date} {random.choice(payees)}\n")
            f.write(f"    {dest}  ${amount:.2f}\n")
            f.write(f"    {income}\n\n")
            posting_count += 2

        elif r < 0.92:
            # Investment purchase
            comm = random.choice(commodities)
            qty = random.randint(1, 50)
            price = round(random.uniform(50, 500), 2)
            f.write(f"{current_date} Buy {comm}\n")
            f.write(f"    Assets:Investment:Brokerage  {qty} {comm} @ ${price:.2f}\n")
            f.write(f"    Assets:Checking\n\n")
            posting_count += 2

        else:
            # 4-posting transaction with tags
            payee = random.choice(payees)
            exps = random.sample(accounts_expense, 3)
            source = random.choice(accounts_asset[:2])
            amounts = [round(random.uniform(5, 100), 2) for _ in range(3)]
            f.write(f"{current_date} {payee}\n")
            f.write(f"    ; :tag1:\n")
            for exp, amt in zip(exps, amounts):
                f.write(f"    {exp}  ${amt:.2f}\n")
            f.write(f"    {source}\n\n")
            posting_count += 4

print(f"Generated {posting_count} postings")
