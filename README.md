# gridlock #

Traverse and explicate grid structures using emacs.

## Why? ##

Just. Because.

## No really. Why? ##

Ever been lost in a large .csv file and wished you knew what heading your cursor was on?

Ever been trolling around a fix log file and wondered what tag *that* was?

Amaze onlookers with your effortless aplomb.

Inspire envy from other would-be studiers of structured data sadly frustrated by inscrutable format.

`csv-mode` is nice and all, and `csv-align-fields` is a perfectly nice method to make fields align.  But it doesn't help when the size and number of fields exceeds the size of your window.  Then you are, to be technical, SOL.  But here comes `gridlock-mode` to the rescue.  It gives you the flexibility to move between fields on the same line, and to the next or previous line, meanwhile showing a helpful heading detailing where you are.  It is like giving GPS to a caveman, and yes in that scenario you play the part of the neanderthal.  

### Display Formats ###

There are different methods built-in to gridlock to help display where you are.  It comes with:

  * popup
  * pos-tip
  * quick-peek
  * minibuffer echo

## Demonstration ##


## Installation ##

Ensure the gridlock .el files are in your load-path.  Then do something like the following `use-package` stanza:

`(use-package gridlock-csv  
  :after csv-mode  
  :bind (:map csv-mode-map ("C-c C-l" . gridlock-csv-mode)))
`

### Flexibility ###

`gridlock-mode` is also intended to be used in file formats other than csv.  Anything that has a structure than can be described by regular expressions can be attuned to the splendor of gridlock's capabilities.  Here, authors of unforetold minor-modes of data exploration, are the variables with which gridlock's capabilities can be adjusted:

#### gridlock-anchor-regex ####

This regex identifies lines of interest.  Lines without a match will be ignored.  Is every line interesting, like in a typical csv file?  Then set this to its default, "^".  Otherwise, this regex identifies what gridlock considers the anchor point:  the point at which further field processing begins.

#### gridlock-field-delimiter ####

Once you've identified lines of interest, you need to split them up into any fields that may be present.  This regex describes the delimiter that splits fields.  For the typical csv format, the default value is ",".

#### gridlock-field-regex-begin ####

Jewels may lie among cruft.  Even on lines of interest, there may be text before or after the fields: preambles or such.  This regex, if non-empty, describes the point on the line at which the first field begins.  If this regex is left empty, its default, then the fields are presumed to begin immediately after the anchor point.

#### gridlock-field-regex-end ####

Wheat shines among chaff.  This regex identifies the ending of the fields on lines of interest.  If left empty, its default, the field processing extends to the end of the line.

## Commands ##

