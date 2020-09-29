# errorlocate 0.4

* Implemented optimization, only invalid records are now treated. Can greatly
enhance processing time! Thanks to Jos de Waard.

# errorlocate 0.3.3

* Fixed issue #21, thanks to Sander Scholtus: strict equalities
* Fixed issue #22, thanks to Sander Scholtus: missing columns in data.
* Fixed issue #23, "<var> =="" FALSE in if clause was handled incorrectly.

# errorlocate 0.3.0

* Fixed issue #19 and #20: rules now may contain var_group and assignments
* Fixed an issue with soft constraints: type of variables was sometimes incorrect
* Parsing of if statements with more than 2 expressions in the condition is now improved

# errorlocate 0.2.0

* Fixed issue #17: if-rules may contain a linear equality.
