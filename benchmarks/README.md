# Are we fast yet?

Nope.

All charts plotted on a log-10 (vertical) scale, while all tests are done on a log-2 scale (horizontal).  I know, I know…  Vertical axis is time in milliseconds, while the horizontal axis varies over input size.  Lower is better.

## Arithmetic Expression

Unambiguous expression grammar parsing whitespace-free inputs with alternating infix operators and a negation operator once every 7 terms.  Parentheses are included in the grammar, but never exercised.   Input size in this case refers to "number of integers".  All input is of the form `-0+1-2*3/4+5-6*-7`, and so on.  The sample input here is of length 8.

![Parse Time in Milliseconds](https://docs.google.com/spreadsheets/d/1j6TD4znxYKXi9em6XoEEhKybyJx9lzm1xIA2_uvKu2A/pubchart?oid=1206621575&format=image)
