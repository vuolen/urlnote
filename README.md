# [URLNote](https://lennu.dev/urlnote/)

A note sharing service where all the data is stored in the URL.

## How it works

The entered text is read as an array of Unicode codepoints. The maximum numerical value of a codepoint is 0x10FFFF. Then we turn the array into a decimal number by interpreting each codepoint as a single digit of a large, base-10FFFF number. This decimal number is then converted to a base-83 number, which uses the 83 URL-safe characters to represent the number. This encoding algorithm is slow for longer messages, but it is also the most efficient.
