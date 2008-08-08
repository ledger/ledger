/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

using CSVReader;

/**
 * @file ParseCcStmt.cs
 *
 * @brief Provides a .NET way to turn a CSV report into Ledger entries.
 *
 * I use this code for converting the statements from my own credit card
 * issuer.  I realize it's strange for this to be in C#, but I wrote it
 * during a phase of C# contracting.  The code is solid enough now --
 * and the Mono project is portable enough -- that I haven't seen the
 * need to rewrite it into another language like Python.
 */

namespace JohnWiegley
{
  public class Transaction
  {
    public DateTime Date;
    public DateTime PostedDate;
    public string   Code;
    public string   Payee;
    public Decimal  Amount;
  }

  public interface IStatementConverter
  {
    List<Transaction> ConvertRecords(Stream s);
  }

  public class ConvertGoldMasterCardStatement : IStatementConverter
  {
    public List<Transaction> ConvertRecords(Stream s)
    {
      List<Transaction> xacts = new List<Transaction>();

      using (CSVReader.CSVReader csv = new CSVReader.CSVReader(s)) {
	string[] fields;
	while ((fields = csv.GetCSVLine()) != null) {
	  if (fields[0] == "TRANSACTION DATE")
	    continue;
					
	  Transaction xact = new Transaction();

	  xact.Date	  = DateTime.ParseExact(fields[0], "mm/dd/yy", null);
	  xact.PostedDate = DateTime.ParseExact(fields[1], "mm/dd/yy", null);
	  xact.Payee	  = fields[2].Trim();
	  xact.Code	  = fields[3].Trim();
	  xact.Amount	  = Convert.ToDecimal(fields[4].Trim());

	  if (xact.Code.Length == 0)
	    xact.Code = null;

	  xacts.Add(xact);
	}
      }
      return xacts;
    }
  }
	
  public class ConvertMastercardStatement : IStatementConverter
  {
    public List<Transaction> ConvertRecords(Stream s)
    {
      List<Transaction> xacts = new List<Transaction>();

      using (CSVReader.CSVReader csv = new CSVReader.CSVReader(s)) {
	string[] fields;
	while ((fields = csv.GetCSVLine()) != null) {
	  Transaction xact = new Transaction();

	  xact.Date   = DateTime.ParseExact(fields[0], "m/dd/yyyy", null);
	  xact.Payee  = fields[2].Trim();
	  xact.Code   = fields[3].Trim();
	  xact.Amount = - Convert.ToDecimal(fields[4].Trim());

	  if (xact.Code.Length == 0)
	    xact.Code = null;

	  xacts.Add(xact);
	}
      }
      return xacts;
    }
  }

  public class PrintTransactions
  {
    public string DefaultAccount(Transaction xact) {
      if (Regex.IsMatch(xact.Payee, "IGA"))
	return "Expenses:Food";
      return "Expenses:Food";
    }
		
    public void Print(string AccountName, string PayAccountName,
		      List<Transaction> xacts)
    {
      foreach (Transaction xact in xacts) {
	if (xact.Amount < 0) {
	  Console.WriteLine("{0} * {1}{2}", xact.Date.ToString("yyyy/mm/dd"),
			    xact.Code != null ? "(" + xact.Code + ") " : "",
			    xact.Payee);
	  Console.WriteLine("    {0,-36}{1,12}", AccountName,
			    "$" + (- xact.Amount).ToString());
	  Console.WriteLine("    {0}", PayAccountName);
	} else {
	  Console.WriteLine("{0} {1}{2}", xact.Date.ToString("yyyy/mm/dd"),
			    xact.Code != null ? "(" + xact.Code + ") " : "",
			    xact.Payee);
	  Console.WriteLine("    {0,-36}{1,12}", DefaultAccount(xact),
			    "$" + xact.Amount.ToString());
	  Console.WriteLine("    * {0}", AccountName);
	}
	Console.WriteLine();
      }
    }
  }

  public class ParseCcStmt
  {
    public static int Main(string[] args)
    {
      StreamReader reader = new StreamReader(args[0]);
      string firstLine = reader.ReadLine();

      string CardAccount = args[1];
      string BankAccount = args[2];

      IStatementConverter converter;

      if (firstLine.StartsWith("TRANSACTION DATE")) {
	converter = new ConvertGoldMasterCardStatement();
      } else {
	converter = new ConvertMastercardStatement();
      }

      reader = new StreamReader(args[0]);
      List<Transaction> xacts = converter.ConvertRecords(reader.BaseStream);

      PrintTransactions printer = new PrintTransactions();
      printer.Print(CardAccount, BankAccount, xacts);

      return 0;
    }
  }
}
