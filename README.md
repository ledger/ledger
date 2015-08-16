# UTF8-CPP: UTF-8 with C++ in a Portable Way

[The Sourceforge project page](https://sourceforge.net/projects/utfcpp)

<div id="toc">

## Table of Contents

*   [Introduction](#introduction)
*   [Examples of Use](#examples)
    *   [Introductionary Sample](#introsample)
    *   [Checking if a file contains valid UTF-8 text](#validfile)
    *   [Ensure that a string contains valid UTF-8 text](#fixinvalid)
*   [Reference](#reference)
    *   [Functions From utf8 Namespace](#funutf8)
    *   [Types From utf8 Namespace](#typesutf8)
    *   [Functions From utf8::unchecked Namespace](#fununchecked)
    *   [Types From utf8::unchecked Namespace](#typesunchecked)
*   [Points of Interest](#points)
*   [Links](#links)

</div>

## Introduction

Many C++ developers miss an easy and portable way of handling Unicode encoded strings. The original C++ Standard (known as C++98 or C++03) is Unicode agnostic. C++11 provides some support for Unicode on core language and library level: u8, u, and U character and string literals, char16_t and char32_t character types, u16string and u32string library classes, and codecvt support for conversions between Unicode encoding forms. In the meantime, developers use third party libraries like ICU, OS specific capabilities, or simply roll out their own solutions.

In order to easily handle UTF-8 encoded Unicode strings, I came up with a small generic library. For anybody used to work with STL algorithms and iterators, it should be easy and natural to use. The code is freely available for any purpose - check out the license at the beginning of the utf8.h file. If you run into bugs or performance issues, please let me know and I'll do my best to address them.

The purpose of this article is not to offer an introduction to Unicode in general, and UTF-8 in particular. If you are not familiar with Unicode, be sure to check out [Unicode Home Page](http://www.unicode.org/) or some other source of information for Unicode. Also, it is not my aim to advocate the use of UTF-8 encoded strings in C++ programs; if you want to handle UTF-8 encoded strings from C++, I am sure you have good reasons for it.

## Examples of use

### Introductionary Sample

To illustrate the use of the library, let's start with a small but complete program that opens a file containing UTF-8 encoded text, reads it line by line, checks each line for invalid UTF-8 byte sequences, and converts it to UTF-16 encoding and back to UTF-8:

<pre><span class="preprocessor">#include <fstream></span>
<span class="preprocessor">#include <iostream></span>
<span class="preprocessor">#include <string></span>
<span class="preprocessor">#include <vector></span>
<span class="preprocessor">#include "utf8.h"</span>
<span class="keyword">using namespace</span> std;
<span class="keyword">int</span> main(<span class="keyword">int</span> argc, <span class="keyword">char</span>** argv)
{
    <span class="keyword">if</span> (argc != <span class="literal">2</span>) {
        cout << <span class="literal">"\nUsage: docsample filename\n"</span>;
        <span class="keyword">return</span> <span class="literal">0</span>;
    }

    <span class="keyword">const char</span>* test_file_path = argv[1];
    <span class="comment">// Open the test file (contains UTF-8 encoded text)</span>
    ifstream fs8(test_file_path);
    <span class="keyword">if</span> (!fs8.is_open()) {
    cout << <span class="literal">"Could not open "</span> << test_file_path << endl;
    <span class="keyword">return</span> <span class="literal">0</span>;
    }

    <span class="keyword">unsigned</span> line_count = <span class="literal">1</span>;
    string line;
    <span class="comment">// Play with all the lines in the file</span>
    <span class="keyword">while</span> (getline(fs8, line)) {
       <span class="comment">// check for invalid utf-8 (for a simple yes/no check, there is also utf8::is_valid function)</span>
        string::iterator end_it = utf8::find_invalid(line.begin(), line.end());
        <span class="keyword">if</span> (end_it != line.end()) {
            cout << <span class="literal">"Invalid UTF-8 encoding detected at line "</span> << line_count << <span class="literal">"\n"</span>;
            cout << <span class="literal">"This part is fine: "</span> << string(line.begin(), end_it) << <span class="literal">"\n"</span>;
        }

        <span class="comment">// Get the line length (at least for the valid part)</span>
        <span class="keyword">int</span> length = utf8::distance(line.begin(), end_it);
        cout << <span class="literal">"Length of line "</span> << line_count << <span class="literal">" is "</span> << length <<  <span class="literal">"\n"</span>;

        <span class="comment">// Convert it to utf-16</span>
        vector<unsigned short> utf16line;
        utf8::utf8to16(line.begin(), end_it, back_inserter(utf16line));

        <span class="comment">// And back to utf-8</span>
        string utf8line; 
        utf8::utf16to8(utf16line.begin(), utf16line.end(), back_inserter(utf8line));

        <span class="comment">// Confirm that the conversion went OK:</span>
        <span class="keyword">if</span> (utf8line != string(line.begin(), end_it))
            cout << <span class="literal">"Error in UTF-16 conversion at line: "</span> << line_count << <span class="literal">"\n"</span>;        

        line_count++;
    }
    <span class="keyword">return</span> <span class="literal">0</span>;
}
</pre>

In the previous code sample, for each line we performed a detection of invalid UTF-8 sequences with `find_invalid`; the number of characters (more precisely - the number of Unicode code points, including the end of line and even BOM if there is one) in each line was determined with a use of `utf8::distance`; finally, we have converted each line to UTF-16 encoding with `utf8to16` and back to UTF-8 with `utf16to8`.

### Checking if a file contains valid UTF-8 text

Here is a function that checks whether the content of a file is valid UTF-8 encoded text without reading the content into the memory:

<pre>    
<span class="keyword">bool</span> valid_utf8_file(i<span class="keyword">const char</span>* file_name)
{
    ifstream ifs(file_name);
    <span class="keyword">if</span> (!ifs)
        <span class="keyword">return false</span>; <span class="comment">// even better, throw here</span>

    istreambuf_iterator<<span class="keyword">char</span>> it(ifs.rdbuf());
    istreambuf_iterator<<span class="keyword">char</span>> eos;

    <span class="keyword">return</span> utf8::is_valid(it, eos);
}
</pre>

Because the function `utf8::is_valid()` works with input iterators, we were able to pass an `istreambuf_iterator` to it and read the content of the file directly without loading it to the memory first.

Note that other functions that take input iterator arguments can be used in a similar way. For instance, to read the content of a UTF-8 encoded text file and convert the text to UTF-16, just do something like:

<pre>    utf8::utf8to16(it, eos, back_inserter(u16string));
</pre>

### Ensure that a string contains valid UTF-8 text

If we have some text that "probably" contains UTF-8 encoded text and we want to replace any invalid UTF-8 sequence with a replacement character, something like the following function may be used:

<pre><span class="keyword">void</span> fix_utf8_string(std::string& str)
{
    std::string temp;
    utf8::replace_invalid(str.begin(), str.end(), back_inserter(temp));
    str = temp;
}
</pre>

The function will replace any invalid UTF-8 sequence with a Unicode replacement character. There is an overloaded function that enables the caller to supply their own replacement character.

## Reference

### Functions From utf8 Namespace

#### utf8::append

Available in version 1.0 and later.

Encodes a 32 bit code point as a UTF-8 sequence of octets and appends the sequence to a UTF-8 string.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
octet_iterator append(uint32_t cp, octet_iterator result);

</pre>

`octet_iterator`: an output iterator.  
 `cp`: a 32 bit integer representing a code point to append to the sequence.  
 `result`: an output iterator to the place in the sequence where to append the code point.  
 <span class="return_value">Return value</span>: an iterator pointing to the place after the newly appended sequence.

Example of use:

<pre><span class="keyword">unsigned char</span> u[<span class="literal">5</span>] = {<span class="literal">0</span>,<span class="literal">0</span>,<span class="literal">0</span>,<span class="literal">0</span>,<span class="literal">0</span>};
<span class="keyword">unsigned char</span>* end = append(<span class="literal">0x0448</span>, u);
assert (u[<span class="literal">0</span>] == <span class="literal">0xd1</span> && u[<span class="literal">1</span>] == <span class="literal">0x88</span> && u[<span class="literal">2</span>] == <span class="literal">0</span> && u[<span class="literal">3</span>] == <span class="literal">0</span> && u[<span class="literal">4</span>] == <span class="literal">0</span>);
</pre>

Note that `append` does not allocate any memory - it is the burden of the caller to make sure there is enough memory allocated for the operation. To make things more interesting, `append` can add anywhere between 1 and 4 octets to the sequence. In practice, you would most often want to use `std::back_inserter` to ensure that the necessary memory is allocated.

In case of an invalid code point, a `utf8::invalid_code_point` exception is thrown.

#### utf8::next

Available in version 1.0 and later.

Given the iterator to the beginning of the UTF-8 sequence, it returns the code point and moves the iterator to the next position.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
uint32_t next(octet_iterator& it, octet_iterator end);

</pre>

`octet_iterator`: an input iterator.  
 `it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the beginning of the next code point.  
 `end`: end of the UTF-8 sequence to be processed. If `it` gets equal to `end` during the extraction of a code point, an `utf8::not_enough_room` exception is thrown.  
 <span class="return_value">Return value</span>: the 32 bit representation of the processed UTF-8 code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars;
<span class="keyword">int</span> cp = next(w, twochars + <span class="literal">6</span>);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars + <span class="literal">3</span>);
</pre>

This function is typically used to iterate through a UTF-8 encoded string.

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.

#### utf8::peek_next

Available in version 2.1 and later.

Given the iterator to the beginning of the UTF-8 sequence, it returns the code point for the following sequence without changing the value of the iterator.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
uint32_t peek_next(octet_iterator it, octet_iterator end);

</pre>

`octet_iterator`: an input iterator.  
 `it`: an iterator pointing to the beginning of an UTF-8 encoded code point.  
 `end`: end of the UTF-8 sequence to be processed. If `it` gets equal to `end` during the extraction of a code point, an `utf8::not_enough_room` exception is thrown.  
 <span class="return_value">Return value</span>: the 32 bit representation of the processed UTF-8 code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars;
<span class="keyword">int</span> cp = peek_next(w, twochars + <span class="literal">6</span>);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars);
</pre>

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown.

#### utf8::prior

Available in version 1.02 and later.

Given a reference to an iterator pointing to an octet in a UTF-8 sequence, it decreases the iterator until it hits the beginning of the previous UTF-8 encoded code point and returns the 32 bits representation of the code point.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
uint32_t prior(octet_iterator& it, octet_iterator start);

</pre>

`octet_iterator`: a bidirectional iterator.  
 `it`: a reference pointing to an octet within a UTF-8 encoded string. After the function returns, it is decremented to point to the beginning of the previous code point.  
 `start`: an iterator to the beginning of the sequence where the search for the beginning of a code point is performed. It is a safety measure to prevent passing the beginning of the string in the search for a UTF-8 lead octet.  
 <span class="return_value">Return value</span>: the 32 bit representation of the previous code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">unsigned char</span>* w = twochars + <span class="literal">3</span>;
<span class="keyword">int</span> cp = prior (w, twochars);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars);
</pre>

This function has two purposes: one is two iterate backwards through a UTF-8 encoded string. Note that it is usually a better idea to iterate forward instead, since `utf8::next` is faster. The second purpose is to find a beginning of a UTF-8 sequence if we have a random position within a string. Note that in that case `utf8::prior` may not detect an invalid UTF-8 sequence in some scenarios: for instance if there are superfluous trail octets, it will just skip them.

`it` will typically point to the beginning of a code point, and `start` will point to the beginning of the string to ensure we don't go backwards too far. `it` is decreased until it points to a lead UTF-8 octet, and then the UTF-8 sequence beginning with that octet is decoded to a 32 bit representation and returned.

In case `start` is reached before a UTF-8 lead octet is hit, or if an invalid UTF-8 sequence is started by the lead octet, an `invalid_utf8` exception is thrown.

In case `start` equals `it`, a `not_enough_room` exception is thrown.

#### utf8::previous

Deprecated in version 1.02 and later.

Given a reference to an iterator pointing to an octet in a UTF-8 seqence, it decreases the iterator until it hits the beginning of the previous UTF-8 encoded code point and returns the 32 bits representation of the code point.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
uint32_t previous(octet_iterator& it, octet_iterator pass_start);

</pre>

`octet_iterator`: a random access iterator.  
 `it`: a reference pointing to an octet within a UTF-8 encoded string. After the function returns, it is decremented to point to the beginning of the previous code point.  
 `pass_start`: an iterator to the point in the sequence where the search for the beginning of a code point is aborted if no result was reached. It is a safety measure to prevent passing the beginning of the string in the search for a UTF-8 lead octet.  
 <span class="return_value">Return value</span>: the 32 bit representation of the previous code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">unsigned char</span>* w = twochars + <span class="literal">3</span>;
<span class="keyword">int</span> cp = previous (w, twochars - <span class="literal">1</span>);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars);
</pre>

`utf8::previous` is deprecated, and `utf8::prior` should be used instead, although the existing code can continue using this function. The problem is the parameter `pass_start` that points to the position just before the beginning of the sequence. Standard containers don't have the concept of "pass start" and the function can not be used with their iterators.

`it` will typically point to the beginning of a code point, and `pass_start` will point to the octet just before the beginning of the string to ensure we don't go backwards too far. `it` is decreased until it points to a lead UTF-8 octet, and then the UTF-8 sequence beginning with that octet is decoded to a 32 bit representation and returned.

In case `pass_start` is reached before a UTF-8 lead octet is hit, or if an invalid UTF-8 sequence is started by the lead octet, an `invalid_utf8` exception is thrown

#### utf8::advance

Available in version 1.0 and later.

Advances an iterator by the specified number of code points within an UTF-8 sequence.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, typename distance_type> 
<span class="keyword">void</span> advance (octet_iterator& it, distance_type n, octet_iterator end);

</pre>

`octet_iterator`: an input iterator.  
 `distance_type`: an integral type convertible to `octet_iterator`'s difference type.  
 `it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the nth following code point.  
 `n`: a positive integer that shows how many code points we want to advance.  
 `end`: end of the UTF-8 sequence to be processed. If `it` gets equal to `end` during the extraction of a code point, an `utf8::not_enough_room` exception is thrown.  

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">unsigned char</span>* w = twochars;
advance (w, <span class="literal">2</span>, twochars + <span class="literal">6</span>);
assert (w == twochars + <span class="literal">5</span>);
</pre>

This function works only "forward". In case of a negative `n`, there is no effect.

In case of an invalid code point, a `utf8::invalid_code_point` exception is thrown.

#### utf8::distance

Available in version 1.0 and later.

Given the iterators to two UTF-8 encoded code points in a seqence, returns the number of code points between them.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
<span class="keyword">typename</span> std::iterator_traits<octet_iterator>::difference_type distance (octet_iterator first, octet_iterator last);

</pre>

`octet_iterator`: an input iterator.  
 `first`: an iterator to a beginning of a UTF-8 encoded code point.  
 `last`: an iterator to a "post-end" of the last UTF-8 encoded code point in the sequence we are trying to determine the length. It can be the beginning of a new code point, or not.  
 <span class="return_value">Return value</span> the distance between the iterators, in code points.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
size_t dist = utf8::distance(twochars, twochars + <span class="literal">5</span>);
assert (dist == <span class="literal">2</span>);
</pre>

This function is used to find the length (in code points) of a UTF-8 encoded string. The reason it is called _distance_, rather than, say, _length_ is mainly because developers are used that _length_ is an O(1) function. Computing the length of an UTF-8 string is a linear operation, and it looked better to model it after `std::distance` algorithm.

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown. If `last` does not point to the past-of-end of a UTF-8 seqence, a `utf8::not_enough_room` exception is thrown.

#### utf8::utf16to8

Available in version 1.0 and later.

Converts a UTF-16 encoded string to UTF-8.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> u16bit_iterator, <span class="keyword">typename</span> octet_iterator>
octet_iterator utf16to8 (u16bit_iterator start, u16bit_iterator end, octet_iterator result);

</pre>

`u16bit_iterator`: an input iterator.  
 `octet_iterator`: an output iterator.  
 `start`: an iterator pointing to the beginning of the UTF-16 encoded string to convert.  
 `end`: an iterator pointing to pass-the-end of the UTF-16 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

<pre><span class="keyword">unsigned short</span> utf16string[] = {<span class="literal">0x41</span>, <span class="literal">0x0448</span>, <span class="literal">0x65e5</span>, <span class="literal">0xd834</span>, <span class="literal">0xdd1e</span>};
vector<<span class="keyword">unsigned char</span>> utf8result;
utf16to8(utf16string, utf16string + <span class="literal">5</span>, back_inserter(utf8result));
assert (utf8result.size() == <span class="literal">10</span>);    
</pre>

In case of invalid UTF-16 sequence, a `utf8::invalid_utf16` exception is thrown.

#### utf8::utf8to16

Available in version 1.0 and later.

Converts an UTF-8 encoded string to UTF-16

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> u16bit_iterator, typename octet_iterator>
u16bit_iterator utf8to16 (octet_iterator start, octet_iterator end, u16bit_iterator result);

</pre>

`octet_iterator`: an input iterator.  
 `u16bit_iterator`: an output iterator.  
 `start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert. < br /> `end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-16 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-16 string.

Example of use:

<pre><span class="keyword">char</span> utf8_with_surrogates[] = <span class="literal">"\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e"</span>;
vector <<span class="keyword">unsigned short</span>> utf16result;
utf8to16(utf8_with_surrogates, utf8_with_surrogates + <span class="literal">9</span>, back_inserter(utf16result));
assert (utf16result.size() == <span class="literal">4</span>);
assert (utf16result[<span class="literal">2</span>] == <span class="literal">0xd834</span>);
assert (utf16result[<span class="literal">3</span>] == <span class="literal">0xdd1e</span>);
</pre>

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown. If `end` does not point to the past-of-end of a UTF-8 seqence, a `utf8::not_enough_room` exception is thrown.

#### utf8::utf32to8

Available in version 1.0 and later.

Converts a UTF-32 encoded string to UTF-8.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, typename u32bit_iterator>
octet_iterator utf32to8 (u32bit_iterator start, u32bit_iterator end, octet_iterator result);

</pre>

`octet_iterator`: an output iterator.  
 `u32bit_iterator`: an input iterator.  
 `start`: an iterator pointing to the beginning of the UTF-32 encoded string to convert.  
 `end`: an iterator pointing to pass-the-end of the UTF-32 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

<pre><span class="keyword">int</span> utf32string[] = {<span class="literal">0x448</span>, <span class="literal">0x65E5</span>, <span class="literal">0x10346</span>, <span class="literal">0</span>};
vector<<span class="keyword">unsigned char</span>> utf8result;
utf32to8(utf32string, utf32string + <span class="literal">3</span>, back_inserter(utf8result));
assert (utf8result.size() == <span class="literal">9</span>);
</pre>

In case of invalid UTF-32 string, a `utf8::invalid_code_point` exception is thrown.

#### utf8::utf8to32

Available in version 1.0 and later.

Converts a UTF-8 encoded string to UTF-32.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, <span class="keyword">typename</span> u32bit_iterator>
u32bit_iterator utf8to32 (octet_iterator start, octet_iterator end, u32bit_iterator result);

</pre>

`octet_iterator`: an input iterator.  
 `u32bit_iterator`: an output iterator.  
 `start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert.  
 `end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-32 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-32 string.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
vector<<span class="keyword">int</span>> utf32result;
utf8to32(twochars, twochars + <span class="literal">5</span>, back_inserter(utf32result));
assert (utf32result.size() == <span class="literal">2</span>);
</pre>

In case of an invalid UTF-8 seqence, a `utf8::invalid_utf8` exception is thrown. If `end` does not point to the past-of-end of a UTF-8 seqence, a `utf8::not_enough_room` exception is thrown.

#### utf8::find_invalid

Available in version 1.0 and later.

Detects an invalid sequence within a UTF-8 string.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
octet_iterator find_invalid(octet_iterator start, octet_iterator end);
</pre>

`octet_iterator`: an input iterator.  
 `start`: an iterator pointing to the beginning of the UTF-8 string to test for validity.  
 `end`: an iterator pointing to pass-the-end of the UTF-8 string to test for validity.  
 <span class="return_value">Return value</span>: an iterator pointing to the first invalid octet in the UTF-8 string. In case none were found, equals `end`.

Example of use:

<pre><span class="keyword">char</span> utf_invalid[] = <span class="literal">"\xe6\x97\xa5\xd1\x88\xfa"</span>;
<span class="keyword">char</span>* invalid = find_invalid(utf_invalid, utf_invalid + <span class="literal">6</span>);
assert (invalid == utf_invalid + <span class="literal">5</span>);
</pre>

This function is typically used to make sure a UTF-8 string is valid before processing it with other functions. It is especially important to call it if before doing any of the _unchecked_ operations on it.

#### utf8::is_valid

Available in version 1.0 and later.

Checks whether a sequence of octets is a valid UTF-8 string.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
<span class="keyword">bool</span> is_valid(octet_iterator start, octet_iterator end);

</pre>

`octet_iterator`: an input iterator.  
 `start`: an iterator pointing to the beginning of the UTF-8 string to test for validity.  
 `end`: an iterator pointing to pass-the-end of the UTF-8 string to test for validity.  
 <span class="return_value">Return value</span>: `true` if the sequence is a valid UTF-8 string; `false` if not.

Example of use:

<pre><span class="keyword">char</span> utf_invalid[] = <span class="literal">"\xe6\x97\xa5\xd1\x88\xfa"</span>;
<span class="keyword">bool</span> bvalid = is_valid(utf_invalid, utf_invalid + <span class="literal">6</span>);
assert (bvalid == false);
</pre>

`is_valid` is a shorthand for `find_invalid(start, end) == end;`. You may want to use it to make sure that a byte seqence is a valid UTF-8 string without the need to know where it fails if it is not valid.

#### utf8::replace_invalid

Available in version 2.0 and later.

Replaces all invalid UTF-8 sequences within a string with a replacement marker.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, <span class="keyword">typename</span> output_iterator>
output_iterator replace_invalid(octet_iterator start, octet_iterator end, output_iterator out, uint32_t replacement);
<span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, <span class="keyword">typename</span> output_iterator>
output_iterator replace_invalid(octet_iterator start, octet_iterator end, output_iterator out);

</pre>

`octet_iterator`: an input iterator.  
 `output_iterator`: an output iterator.  
 `start`: an iterator pointing to the beginning of the UTF-8 string to look for invalid UTF-8 sequences.  
 `end`: an iterator pointing to pass-the-end of the UTF-8 string to look for invalid UTF-8 sequences.  
 `out`: An output iterator to the range where the result of replacement is stored.  
 `replacement`: A Unicode code point for the replacement marker. The version without this parameter assumes the value `0xfffd`  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the UTF-8 string with replaced invalid sequences.

Example of use:

<pre><span class="keyword">char</span> invalid_sequence[] = <span class="literal">"a\x80\xe0\xa0\xc0\xaf\xed\xa0\x80z"</span>;
vector<<span class="keyword">char</span>> replace_invalid_result;
replace_invalid (invalid_sequence, invalid_sequence + sizeof(invalid_sequence), back_inserter(replace_invalid_result), <span class="literal">'?'</span>);
bvalid = is_valid(replace_invalid_result.begin(), replace_invalid_result.end());
assert (bvalid);
<span class="keyword">char</span>* fixed_invalid_sequence = <span class="literal">"a????z"</span>;
assert (std::equal(replace_invalid_result.begin(), replace_invalid_result.end(), fixed_invalid_sequence));
</pre>

`replace_invalid` does not perform in-place replacement of invalid sequences. Rather, it produces a copy of the original string with the invalid sequences replaced with a replacement marker. Therefore, `out` must not be in the `[start, end]` range.

If `end` does not point to the past-of-end of a UTF-8 sequence, a `utf8::not_enough_room` exception is thrown.

#### utf8::starts_with_bom

Available in version 2.3 and later. Relaces deprecated `is_bom()` function.

Checks whether an octet sequence starts with a UTF-8 byte order mark (BOM)

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
<span class="keyword">bool</span> starts_with_bom (octet_iterator it, octet_iterator end);
</pre>

`octet_iterator`: an input iterator.  
 `it`: beginning of the octet sequence to check  
 `end`: pass-end of the sequence to check  
 <span class="return_value">Return value</span>: `true` if the sequence starts with a UTF-8 byte order mark; `false` if not.

Example of use:

<pre><span class="keyword">unsigned char</span> byte_order_mark[] = {<span class="literal">0xef</span>, <span class="literal">0xbb</span>, <span class="literal">0xbf</span>};
<span class="keyword">bool</span> bbom = starts_with_bom(byte_order_mark, byte_order_mark + <span class="keyword">sizeof</span>(byte_order_mark));
assert (bbom == <span class="literal">true</span>);
</pre>

The typical use of this function is to check the first three bytes of a file. If they form the UTF-8 BOM, we want to skip them before processing the actual UTF-8 encoded text.

#### utf8::is_bom

Available in version 1.0 and later. Deprecated in version 2.3\. `starts_with_bom()` should be used instead.

Checks whether a sequence of three octets is a UTF-8 byte order mark (BOM)

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator> 
<span class="keyword">bool</span> is_bom (octet_iterator it); <span class="comment"> // Deprecated</span>
</pre>

`octet_iterator`: an input iterator.  
 `it`: beginning of the 3-octet sequence to check  
 <span class="return_value">Return value</span>: `true` if the sequence is UTF-8 byte order mark; `false` if not.

Example of use:

<pre><span class="keyword">unsigned char</span> byte_order_mark[] = {<span class="literal">0xef</span>, <span class="literal">0xbb</span>, <span class="literal">0xbf</span>};
<span class="keyword">bool</span> bbom = is_bom(byte_order_mark);
assert (bbom == <span class="literal">true</span>);
</pre>

The typical use of this function is to check the first three bytes of a file. If they form the UTF-8 BOM, we want to skip them before processing the actual UTF-8 encoded text.

If a sequence is shorter than three bytes, an invalid iterator will be dereferenced. Therefore, this function is deprecated in favor of `starts_with_bom()`that takes the end of sequence as an argument.

### Types From utf8 Namespace

#### utf8::exception

Available in version 2.3 and later.

Base class for the exceptions thrown by UTF CPP library functions.

<pre><span class="keyword">class</span> exception : <span class="keyword">public</span> std::exception {};
</pre>

Example of use:

<pre><span class="keyword">try</span> {
  code_that_uses_utf_cpp_library();
}
<span class="keyword">catch</span>(<span class="keyword">const</span> utf8::exception& utfcpp_ex) {
  cerr << utfcpp_ex.what();
}
</pre>

#### utf8::invalid_code_point

Available in version 1.0 and later.

Thrown by UTF8 CPP functions such as `advance` and `next` if an UTF-8 sequence represents and invalid code point.

<pre><span class="keyword">class</span> invalid_code_point : <span class="keyword">public</span> exception {
<span class="keyword">public</span>: 
    uint32_t code_point() <span class="keyword">const</span>;
};

</pre>

Member function `code_point()` can be used to determine the invalid code point that caused the exception to be thrown.

#### utf8::invalid_utf8

Available in version 1.0 and later.

Thrown by UTF8 CPP functions such as `next` and `prior` if an invalid UTF-8 sequence is detected during decoding.

<pre><span class="keyword">class</span> invalid_utf8 : <span class="keyword">public</span> exception {
<span class="keyword">public</span>: 
    uint8_t utf8_octet() <span class="keyword">const</span>;
};
</pre>

Member function `utf8_octet()` can be used to determine the beginning of the byte sequence that caused the exception to be thrown.

#### utf8::invalid_utf16

Available in version 1.0 and later.

Thrown by UTF8 CPP function `utf16to8` if an invalid UTF-16 sequence is detected during decoding.

<pre><span class="keyword">class</span> invalid_utf16 : <span class="keyword">public</span> exception {
<span class="keyword">public</span>: 
    uint16_t utf16_word() <span class="keyword">const</span>;
};
</pre>

Member function `utf16_word()` can be used to determine the UTF-16 code unit that caused the exception to be thrown.

#### utf8::not_enough_room

Available in version 1.0 and later.

Thrown by UTF8 CPP functions such as `next` if the end of the decoded UTF-8 sequence was reached before the code point was decoded.

<pre><span class="keyword">class</span> not_enough_room : <span class="keyword">public</span> exception {};
</pre>

#### utf8::iterator

Available in version 2.0 and later.

Adapts the underlying octet iterator to iterate over the sequence of code points, rather than raw octets.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
<span class="keyword">class</span> iterator;
</pre>

##### Member functions

<dl>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

</dl>

Example of use:

<pre><span class="keyword">char</span>* threechars = <span class="literal">"\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88"</span>;
utf8::iterator<<span class="keyword">char</span>*> it(threechars, threechars, threechars + <span class="literal">9</span>);
utf8::iterator<<span class="keyword">char</span>*> it2 = it;
assert (it2 == it);
assert (*it == <span class="literal">0x10346</span>);
assert (*(++it) == <span class="literal">0x65e5</span>);
assert ((*it++) == <span class="literal">0x65e5</span>);
assert (*it == <span class="literal">0x0448</span>);
assert (it != it2);
utf8::iterator<<span class="keyword">char</span>*> endit (threechars + <span class="literal">9</span>, threechars, threechars + <span class="literal">9</span>);  
assert (++it == endit);
assert (*(--it) == <span class="literal">0x0448</span>);
assert ((*it--) == <span class="literal">0x0448</span>);
assert (*it == <span class="literal">0x65e5</span>);
assert (--it == utf8::iterator<<span class="keyword">char</span>*>(threechars, threechars, threechars + <span class="literal">9</span>));
assert (*it == <span class="literal">0x10346</span>);
</pre>

The purpose of `utf8::iterator` adapter is to enable easy iteration as well as the use of STL algorithms with UTF-8 encoded strings. Increment and decrement operators are implemented in terms of `utf8::next()` and `utf8::prior()` functions.

Note that `utf8::iterator` adapter is a checked iterator. It operates on the range specified in the constructor; any attempt to go out of that range will result in an exception. Even the comparison operators require both iterator object to be constructed against the same range - otherwise an exception is thrown. Typically, the range will be determined by sequence container functions `begin` and `end`, i.e.:

<pre>std::string s = <span class="literal">"example"</span>;
utf8::iterator i (s.begin(), s.begin(), s.end());
</pre>

### Functions From utf8::unchecked Namespace

#### utf8::unchecked::append

Available in version 1.0 and later.

Encodes a 32 bit code point as a UTF-8 sequence of octets and appends the sequence to a UTF-8 string.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
octet_iterator append(uint32_t cp, octet_iterator result);

</pre>

`cp`: A 32 bit integer representing a code point to append to the sequence.  
 `result`: An output iterator to the place in the sequence where to append the code point.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the newly appended sequence.

Example of use:

<pre><span class="keyword">unsigned char</span> u[<span class="literal">5</span>] = {<span class="literal">0</span>,<span class="literal">0</span>,<span class="literal">0</span>,<span class="literal">0</span>,<span class="literal">0</span>};
<span class="keyword">unsigned char</span>* end = unchecked::append(<span class="literal">0x0448</span>, u);
assert (u[<span class="literal">0</span>] == <span class="literal">0xd1</span> && u[<span class="literal">1</span>] == <span class="literal">0x88</span> && u[<span class="literal">2</span>] == <span class="literal">0</span> && u[<span class="literal">3</span>] == <span class="literal">0</span> && u[<span class="literal">4</span>] == <span class="literal">0</span>);
</pre>

This is a faster but less safe version of `utf8::append`. It does not check for validity of the supplied code point, and may produce an invalid UTF-8 sequence.

#### utf8::unchecked::next

Available in version 1.0 and later.

Given the iterator to the beginning of a UTF-8 sequence, it returns the code point and moves the iterator to the next position.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
uint32_t next(octet_iterator& it);

</pre>

`it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the beginning of the next code point.  
 <span class="return_value">Return value</span>: the 32 bit representation of the processed UTF-8 code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars;
<span class="keyword">int</span> cp = unchecked::next(w);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars + <span class="literal">3</span>);
</pre>

This is a faster but less safe version of `utf8::next`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::peek_next

Available in version 2.1 and later.

Given the iterator to the beginning of a UTF-8 sequence, it returns the code point.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
uint32_t peek_next(octet_iterator it);

</pre>

`it`: an iterator pointing to the beginning of an UTF-8 encoded code point.  
 <span class="return_value">Return value</span>: the 32 bit representation of the processed UTF-8 code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars;
<span class="keyword">int</span> cp = unchecked::peek_next(w);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars);
</pre>

This is a faster but less safe version of `utf8::peek_next`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::prior

Available in version 1.02 and later.

Given a reference to an iterator pointing to an octet in a UTF-8 seqence, it decreases the iterator until it hits the beginning of the previous UTF-8 encoded code point and returns the 32 bits representation of the code point.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
uint32_t prior(octet_iterator& it);

</pre>

`it`: a reference pointing to an octet within a UTF-8 encoded string. After the function returns, it is decremented to point to the beginning of the previous code point.  
 <span class="return_value">Return value</span>: the 32 bit representation of the previous code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars + <span class="literal">3</span>;
<span class="keyword">int</span> cp = unchecked::prior (w);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars);
</pre>

This is a faster but less safe version of `utf8::prior`. It does not check for validity of the supplied UTF-8 sequence and offers no boundary checking.

#### utf8::unchecked::previous (deprecated, see utf8::unchecked::prior)

Deprecated in version 1.02 and later.

Given a reference to an iterator pointing to an octet in a UTF-8 seqence, it decreases the iterator until it hits the beginning of the previous UTF-8 encoded code point and returns the 32 bits representation of the code point.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
uint32_t previous(octet_iterator& it);

</pre>

`it`: a reference pointing to an octet within a UTF-8 encoded string. After the function returns, it is decremented to point to the beginning of the previous code point.  
 <span class="return_value">Return value</span>: the 32 bit representation of the previous code point.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars + <span class="literal">3</span>;
<span class="keyword">int</span> cp = unchecked::previous (w);
assert (cp == <span class="literal">0x65e5</span>);
assert (w == twochars);
</pre>

The reason this function is deprecated is just the consistency with the "checked" versions, where `prior` should be used instead of `previous`. In fact, `unchecked::previous` behaves exactly the same as `unchecked::prior`

This is a faster but less safe version of `utf8::previous`. It does not check for validity of the supplied UTF-8 sequence and offers no boundary checking.

#### utf8::unchecked::advance

Available in version 1.0 and later.

Advances an iterator by the specified number of code points within an UTF-8 sequence.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, typename distance_type>
<span class="keyword">void</span> advance (octet_iterator& it, distance_type n);

</pre>

`it`: a reference to an iterator pointing to the beginning of an UTF-8 encoded code point. After the function returns, it is incremented to point to the nth following code point.  
 `n`: a positive integer that shows how many code points we want to advance.  

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
<span class="keyword">char</span>* w = twochars;
unchecked::advance (w, <span class="literal">2</span>);
assert (w == twochars + <span class="literal">5</span>);
</pre>

This function works only "forward". In case of a negative `n`, there is no effect.

This is a faster but less safe version of `utf8::advance`. It does not check for validity of the supplied UTF-8 sequence and offers no boundary checking.

#### utf8::unchecked::distance

Available in version 1.0 and later.

Given the iterators to two UTF-8 encoded code points in a seqence, returns the number of code points between them.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
<span class="keyword">typename</span> std::iterator_traits<octet_iterator>::difference_type distance (octet_iterator first, octet_iterator last);
</pre>

`first`: an iterator to a beginning of a UTF-8 encoded code point.  
 `last`: an iterator to a "post-end" of the last UTF-8 encoded code point in the sequence we are trying to determine the length. It can be the beginning of a new code point, or not.  
 <span class="return_value">Return value</span> the distance between the iterators, in code points.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
size_t dist = utf8::unchecked::distance(twochars, twochars + <span class="literal">5</span>);
assert (dist == <span class="literal">2</span>);
</pre>

This is a faster but less safe version of `utf8::distance`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::utf16to8

Available in version 1.0 and later.

Converts a UTF-16 encoded string to UTF-8.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> u16bit_iterator, <span class="keyword">typename</span> octet_iterator>
octet_iterator utf16to8 (u16bit_iterator start, u16bit_iterator end, octet_iterator result);

</pre>

`start`: an iterator pointing to the beginning of the UTF-16 encoded string to convert.  
 `end`: an iterator pointing to pass-the-end of the UTF-16 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

<pre><span class="keyword">unsigned short</span> utf16string[] = {<span class="literal">0x41</span>, <span class="literal">0x0448</span>, <span class="literal">0x65e5</span>, <span class="literal">0xd834</span>, <span class="literal">0xdd1e</span>};
vector<<span class="keyword">unsigned char</span>> utf8result;
unchecked::utf16to8(utf16string, utf16string + <span class="literal">5</span>, back_inserter(utf8result));
assert (utf8result.size() == <span class="literal">10</span>);    
</pre>

This is a faster but less safe version of `utf8::utf16to8`. It does not check for validity of the supplied UTF-16 sequence.

#### utf8::unchecked::utf8to16

Available in version 1.0 and later.

Converts an UTF-8 encoded string to UTF-16

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> u16bit_iterator, typename octet_iterator>
u16bit_iterator utf8to16 (octet_iterator start, octet_iterator end, u16bit_iterator result);

</pre>

`start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert. < br /> `end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-16 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-16 string.

Example of use:

<pre><span class="keyword">char</span> utf8_with_surrogates[] = <span class="literal">"\xe6\x97\xa5\xd1\x88\xf0\x9d\x84\x9e"</span>;
vector <<span class="keyword">unsigned short</span>> utf16result;
unchecked::utf8to16(utf8_with_surrogates, utf8_with_surrogates + <span class="literal">9</span>, back_inserter(utf16result));
assert (utf16result.size() == <span class="literal">4</span>);
assert (utf16result[<span class="literal">2</span>] == <span class="literal">0xd834</span>);
assert (utf16result[<span class="literal">3</span>] == <span class="literal">0xdd1e</span>);
</pre>

This is a faster but less safe version of `utf8::utf8to16`. It does not check for validity of the supplied UTF-8 sequence.

#### utf8::unchecked::utf32to8

Available in version 1.0 and later.

Converts a UTF-32 encoded string to UTF-8.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, <span class="keyword">typename</span> u32bit_iterator>
octet_iterator utf32to8 (u32bit_iterator start, u32bit_iterator end, octet_iterator result);

</pre>

`start`: an iterator pointing to the beginning of the UTF-32 encoded string to convert.  
 `end`: an iterator pointing to pass-the-end of the UTF-32 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-8 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-8 string.

Example of use:

<pre><span class="keyword">int</span> utf32string[] = {<span class="literal">0x448</span>, <span class="literal">0x65e5</span>, <span class="literal">0x10346</span>, <span class="literal">0</span>};
vector<<span class="keyword">unsigned char</span>> utf8result;
utf32to8(utf32string, utf32string + <span class="literal">3</span>, back_inserter(utf8result));
assert (utf8result.size() == <span class="literal">9</span>);
</pre>

This is a faster but less safe version of `utf8::utf32to8`. It does not check for validity of the supplied UTF-32 sequence.

#### utf8::unchecked::utf8to32

Available in version 1.0 and later.

Converts a UTF-8 encoded string to UTF-32.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator, typename u32bit_iterator>
u32bit_iterator utf8to32 (octet_iterator start, octet_iterator end, u32bit_iterator result);

</pre>

`start`: an iterator pointing to the beginning of the UTF-8 encoded string to convert.  
 `end`: an iterator pointing to pass-the-end of the UTF-8 encoded string to convert.  
 `result`: an output iterator to the place in the UTF-32 string where to append the result of conversion.  
 <span class="return_value">Return value</span>: An iterator pointing to the place after the appended UTF-32 string.

Example of use:

<pre><span class="keyword">char</span>* twochars = <span class="literal">"\xe6\x97\xa5\xd1\x88"</span>;
vector<<span class="keyword">int</span>> utf32result;
unchecked::utf8to32(twochars, twochars + <span class="literal">5</span>, back_inserter(utf32result));
assert (utf32result.size() == <span class="literal">2</span>);
</pre>

This is a faster but less safe version of `utf8::utf8to32`. It does not check for validity of the supplied UTF-8 sequence.

### Types From utf8::unchecked Namespace

#### utf8::iterator

Available in version 2.0 and later.

Adapts the underlying octet iterator to iterate over the sequence of code points, rather than raw octets.

<pre><span class="keyword">template</span> <<span class="keyword">typename</span> octet_iterator>
<span class="keyword">class</span> iterator;
</pre>

##### Member functions

<dl>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

<dt>

<dd>

</dl>

Example of use:

<pre><span class="keyword">char</span>* threechars = <span class="literal">"\xf0\x90\x8d\x86\xe6\x97\xa5\xd1\x88"</span>;
utf8::unchecked::iterator<<span class="keyword">char</span>*> un_it(threechars);
utf8::unchecked::iterator<<span class="keyword">char</span>*> un_it2 = un_it;
assert (un_it2 == un_it);
assert (*un_it == <span class="literal">0x10346</span>);
assert (*(++un_it) == <span class="literal">0x65e5</span>);
assert ((*un_it++) == <span class="literal">0x65e5</span>);
assert (*un_it == <span class="literal">0x0448</span>);
assert (un_it != un_it2);
utf8::::unchecked::iterator<<span class="keyword">char</span>*> un_endit (threechars + <span class="literal">9</span>);  
assert (++un_it == un_endit);
assert (*(--un_it) == <span class="literal">0x0448</span>);
assert ((*un_it--) == <span class="literal">0x0448</span>);
assert (*un_it == <span class="literal">0x65e5</span>);
assert (--un_it == utf8::unchecked::iterator<<span class="keyword">char</span>*>(threechars));
assert (*un_it == <span class="literal">0x10346</span>);
</pre>

This is an unchecked version of `utf8::iterator`. It is faster in many cases, but offers no validity or range checks.

## Points of interest

#### Design goals and decisions

The library was designed to be:

1.  Generic: for better or worse, there are many C++ string classes out there, and the library should work with as many of them as possible.
2.  Portable: the library should be portable both accross different platforms and compilers. The only non-portable code is a small section that declares unsigned integers of different sizes: three typedefs. They can be changed by the users of the library if they don't match their platform. The default setting should work for Windows (both 32 and 64 bit), and most 32 bit and 64 bit Unix derivatives.
3.  Lightweight: follow the "pay only for what you use" guideline.
4.  Unintrusive: avoid forcing any particular design or even programming style on the user. This is a library, not a framework.

#### Alternatives

In case you want to look into other means of working with UTF-8 strings from C++, here is the list of solutions I am aware of:

1.  [ICU Library](http://icu.sourceforge.net/). It is very powerful, complete, feature-rich, mature, and widely used. Also big, intrusive, non-generic, and doesn't play well with the Standard Library. I definitelly recommend looking at ICU even if you don't plan to use it.
2.  C++11 language and library features. Still far from complete, and not widely supported by compiler vendors.
3.  [Glib::ustring](http://www.gtkmm.org/gtkmm2/docs/tutorial/html/ch03s04.html). A class specifically made to work with UTF-8 strings, and also feel like `std::string`. If you prefer to have yet another string class in your code, it may be worth a look. Be aware of the licensing issues, though.
4.  Platform dependent solutions: Windows and POSIX have functions to convert strings from one encoding to another. That is only a subset of what my library offers, but if that is all you need it may be good enough.

## Links

1.  [The Unicode Consortium](http://www.unicode.org/).
2.  [ICU Library](http://icu.sourceforge.net/).
3.  [UTF-8 at Wikipedia](http://en.wikipedia.org/wiki/UTF-8)
4.  [UTF-8 and Unicode FAQ for Unix/Linux](http://www.cl.cam.ac.uk/~mgk25/unicode.html)