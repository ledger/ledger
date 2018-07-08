/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
 * @brief Provides a .NET way to turn a CSV report into Ledger transactions.
 *
 * I use this code for converting the statements from my own credit card
 * issuer.  I realize it's strange for this to be in C#, but I wrote it
 * during a phase of C# contracting.  The code is solid enough now --
 * and the Mono project is portable enough -- that I haven't seen the
 * need to rewrite it into another language like Python.
 */

namespace JohnWiegley
{
  public class Posting
  {
    public DateTime Date;
    public DateTime PostedDate;
    public string   Code;
    public string   Payee;
    public Decimal  Amount;
  }

  public interface IStatementConverter
  {
    List<Posting> ConvertRecords(Stream s);
  }

  public class ConvertGoldMasterCardStatement : IStatementConverter
  {
    public List<Posting> ConvertRecords(Stream s)
    {
      List<Posting> posts = new List<Posting>();

      using (CSVReader.CSVReader csv = new CSVReader.CSVReader(s)) {
	string[] fields;
	while ((fields = csv.GetCSVLine()) != null) {
	  if (fields[0] == "POSTING DATE")
	    continue;
					
	  Posting post = new Posting();

	  post.Date	  = DateTime.ParseEpost(fields[0], "mm/dd/yy", null);
	  post.PostedDate = DateTime.ParseEpost(fields[1], "mm/dd/yy", null);
	  post.Payee	  = fields[2].Trim();
	  post.Code	  = fields[3].Trim();
	  post.Amount	  = Convert.ToDecimal(fields[4].Trim());

	  if (post.Code.Length == 0)
	    post.Code = null;

	  posts.Add(post);
	}
      }
      return posts;
    }
  }
	
  public class ConvertMastercardStatement : IStatementConverter
  {
    public List<Posting> ConvertRecords(Stream s)
    {
      List<Posting> posts = new List<Posting>();

      using (CSVReader.CSVReader csv = new CSVReader.CSVReader(s)) {
	string[] fields;
	while ((fields = csv.GetCSVLine()) != null) {
	  Posting post = new Posting();

	  post.Date   = DateTime.ParseEpost(fields[0], "m/dd/yyyy", null);
	  post.Payee  = fields[2].Trim();
	  post.Code   = fields[3].Trim();
	  post.Amount = - Convert.ToDecimal(fields[4].Trim());

	  if (post.Code.Length == 0)
	    post.Code = null;

	  posts.Add(post);
	}
      }
      return posts;
    }
  }

  public class PrintPostings
  {
    public string DefaultAccount(Posting post) {
      if (Regex.IsMatch(post.Payee, "IGA"))
	return "Expenses:Food";
      return "Expenses:Food";
    }
		
    public void Print(string AccountName, string PayAccountName,
		      List<Posting> posts)
    {
      foreach (Posting post in posts) {
	if (post.Amount < 0) {
	  Console.WriteLine("{0} * {1}{2}", post.Date.ToString("yyyy/mm/dd"),
			    post.Code != null ? "(" + post.Code + ") " : "",
			    post.Payee);
	  Console.WriteLine("    {0,-36}{1,12}", AccountName,
			    "$" + (- post.Amount).ToString());
	  Console.WriteLine("    {0}", PayAccountName);
	} else {
	  Console.WriteLine("{0} {1}{2}", post.Date.ToString("yyyy/mm/dd"),
			    post.Code != null ? "(" + post.Code + ") " : "",
			    post.Payee);
	  Console.WriteLine("    {0,-36}{1,12}", DefaultAccount(post),
			    "$" + post.Amount.ToString());
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

      if (firstLine.StartsWith("POSTING DATE")) {
	converter = new ConvertGoldMasterCardStatement();
      } else {
	converter = new ConvertMastercardStatement();
      }

      reader = new StreamReader(args[0]);
      List<Posting> posts = converter.ConvertRecords(reader.BaseStream);

      PrintPostings printer = new PrintPostings();
      printer.Print(CardAccount, BankAccount, posts);

      return 0;
    }
  }
}
