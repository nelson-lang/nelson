# Bolt's Journal

## 2024-07-16 - Initial Entry
**Learning:** The `stringReplace` function in `StringReplace.cpp` is inefficient for non-overlapping replacements when the replacement string is longer than the search string. The current implementation uses a `while` loop with `modifiedString.replace`, which can lead to multiple costly reallocations of the string's internal buffer.
**Action:** For future string replacement optimizations, I will look for opportunities to pre-calculate the final string size and pre-allocate the necessary memory. This will avoid repeated reallocations and improve performance, especially for large strings or frequent replacement operations.
