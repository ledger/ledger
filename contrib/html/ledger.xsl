<?xml version="1.0" encoding="UTF-8"?>

<!--
  ! ledger-cli xml2html XLST
  !
  ! this XLST produces a browsable html page out of the xml files created by
  !
  ! usage:
  !    xsltproc ledger.xsl input.ledger.xml > output.ledger.html
  ! -->

<!--
 ! Copyright (c) 2021 Markus Katharina Brechtel
 !
 ! Permission is hereby granted, free of charge, to any person obtaining a copy
 ! of this software and associated documentation files (the "Software"), to deal
 ! in the Software without restriction, including without limitation the rights
 ! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 ! copies of the Software, and to permit persons to whom the Software is
 ! furnished to do so, subject to the following conditions:
 !
 ! The above copyright notice and this permission notice shall be included in
 ! all copies or substantial portions of the Software.
 !
 ! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 ! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 ! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 ! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 ! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 ! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 ! SOFTWARE.
 ! -->


<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" encoding="UTF-8" indent="yes" doctype-system="about:legacy-compat"/>

  <xsl:template match="/">
    <html>
      <head>
        <style>
          table, th, td {
            border: 1px solid black;
          }
          table {
            border-spacing: 0;
          }
          table th {
            position: sticky;
            top: 0;
            z-index: 1;
            background: #fff;
          }
          #accounts table tr:nth-child(odd) {
            background: #ccc;
          }
          #accounts table tr:nth-child(even) {
            background: #fff;
          }
          .amount-table-cell {
            text-align: right;
          }
          .amount-negative {
            color: #ff0000;
          }
          #accounts,#transactions,.account {
            display: none;
          }
          #accounts:target,#transactions:target,.account:target {
            display:block;
          }
        </style>
        <script>
        </script>
      </head>
      <body>
        <div>
          <a href="#accounts">Accounts</a>&#160;
          <a href="#transactions">Transactions</a>
        </div>
        <xsl:apply-templates select="ledger/accounts"/>
        <xsl:apply-templates select="ledger/transactions"/>
        <xsl:for-each select="ledger/accounts//account">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="accounts">
    <div id="accounts">
      <h1>Accounts</h1>
      <table>
        <thead>
          <tr>
            <th>Account</th>
            <th>Amount</th>
            <th>Total</th>
          </tr>
        </thead>
        <tbody>
          <xsl:for-each select=".//account">
            <xsl:if test="node()">
              <tr>
                <td>
                  <xsl:attribute name="style">padding-left:<xsl:value-of select="count(ancestor::account)"/>em;</xsl:attribute>
                  <a>
                    <xsl:attribute name="href">#account-<xsl:value-of select="@id"/></xsl:attribute>
                    <xsl:value-of select="name"/>
                  </a>

                </td>
                <td class="amount-table-cell"><xsl:apply-templates select="account-amount/amount"/></td>
                <td class="amount-table-cell"><xsl:apply-templates select="account-total/amount"/></td>
              </tr>
            </xsl:if>
          </xsl:for-each>
        </tbody>
      </table>
    </div>
  </xsl:template>

  <xsl:template match="transactions">
    <div id="transactions">
      <h1>All Transactions</h1>
      <table>
        <thead>
          <tr>
            <th>Date</th>
            <th>Payee</th>
            <th>Account</th>
            <th>Amount</th>
            <th>Total</th>
          </tr>
        </thead>
        <tbody>
          <xsl:for-each select="transaction">
            <tr>
              <td>
                <xsl:attribute name="rowspan"><xsl:value-of select="count(postings/posting)+1"/></xsl:attribute>
                <xsl:value-of select="date"/>
              </td>
              <td>
                <xsl:attribute name="rowspan"><xsl:value-of select="count(postings/posting)+1"/></xsl:attribute>
                <xsl:value-of select="payee"/>
              </td>
            </tr>
              <xsl:for-each select="postings/posting">
                <tr>
                    <td>
                      <a>
                        <xsl:attribute name="href">#account-<xsl:value-of select="account/@ref"/></xsl:attribute>
                        <xsl:value-of select="account"/>
                      </a>
                    </td>
                    <td class="amount-table-cell"><xsl:apply-templates select="post-amount/amount"/></td>
                    <td class="amount-table-cell"><xsl:apply-templates select="total/amount"/></td>
                </tr>
              </xsl:for-each>
          </xsl:for-each>
        </tbody>
      </table>
    </div>
  </xsl:template>

  <xsl:template match="account">
    <xsl:if test="node()">
      <div class="account">
        <xsl:variable name="id" select="@id"/>
        <xsl:attribute name="id">account-<xsl:value-of select="$id"/></xsl:attribute>
        <h1>Account: <xsl:value-of select="fullname"/></h1>
        <table>
          <thead>
            <tr>
              <th>Date</th>
              <th>Payee</th>
              <th>Amount</th>
              <th>Total</th>
            </tr>
          </thead>
          <tbody>
            <xsl:for-each select="/ledger/transactions/transaction[postings/posting/account/@ref = $id]">
              <tr>
                <td>
                  <xsl:attribute name="rowspan"><xsl:value-of select="count(postings/posting[account/@ref = $id])+1"/></xsl:attribute>
                  <xsl:value-of select="date"/>
                </td>
                <td>
                  <xsl:attribute name="rowspan"><xsl:value-of select="count(postings/posting[account/@ref = $id])+1"/></xsl:attribute>
                  <xsl:value-of select="payee"/>
                </td>
              </tr>
                <xsl:for-each select="postings/posting[account/@ref = $id]">
                  <tr>
                      <td class="amount-table-cell"><xsl:apply-templates select="post-amount/amount"/></td>
                      <td class="amount-table-cell"><xsl:apply-templates select="total/amount"/></td>
                  </tr>
                </xsl:for-each>
            </xsl:for-each>
          </tbody>
        </table>
      </div>
    </xsl:if>
  </xsl:template>

  <xsl:template match="amount">
    <!--
      flag descriptions:
        P
          The commodity is prefixed to the value.
        S
          The commodity is separated from the value by a space.
        T
          Thousands markers are used to display the amount.
        E (or D?, TODO clarify if the manual is false)
          The format of the amount is European, with period used as a thousands marker, and comma used as the decimal point.
      -->
    <span>
      <xsl:attribute name="class">amount
        <xsl:choose>
          <xsl:when test="not(contains(quantity,'-'))">amount-positive</xsl:when>
          <xsl:otherwise>amount-negative</xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>

      <xsl:if test="contains(commodity/@flags,'P')">
        <span class="symbol"><xsl:value-of select="commodity/symbol"/></span>
        <xsl:if test="contains(commodity/@flags,'S')">&#160;</xsl:if>
      </xsl:if>

      <span class="quantity">
        <xsl:choose>
          <xsl:when test="not(contains(commodity/@flags,'D') or contains(commodity/@flags,'E'))">
            <!-- TODO Thousands markers -->
            <xsl:value-of select="quantity"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="translate(quantity,'.',',')"/>
          </xsl:otherwise>
        </xsl:choose>
      </span>
      <xsl:if test="not(contains(commodity/@flags,'P'))">
        <xsl:if test="contains(commodity/@flags,'S')">&#160;</xsl:if>
        <span class="symbol"><xsl:value-of select="commodity/symbol"/></span>
      </xsl:if>
    </span>
  </xsl:template>

</xsl:stylesheet>
