# dtext-mode.el

A major mode in Emacs for editing Danbooru DText.

DText is the markup language used on Danbooru and other Booru
sites. It implements syntax highlighting and keyboard commands for
inserting basic formatting tags.

To activate this major mode, use `M-x dtext-mode`. To create a
temporary buffer for editing posts, use `M-x dtext-scratch`.


# Install

There are a couple of ways to do it. One way to do it&#x2013;and the easiest&#x2013;is to
download one of the [releases](https://github.com/JohnDevlopment/dtext-mode.el/releases) and install it with `M-x package-install-file`.

Another way to install this is by manually putting `dtext-mode.el` in your
`load-path` and then using `(require 'dtext-mode)`.


# How To Report Bugs

Please use Github Issues to report bugs.


# Supported Tags

Below is a list of DText markup tags and their respective
key-bindings. If you have a selected region, then DText Mode will wrap
the tags around that region.

Each key-binding has three parts: `C-c`, the "group" of said binding,
and a single letter taken from the name of that tag. Each section
below provides a menmonic to help users remember them.


## Common Tags

-   Key-bindings begin with `C-c C-t`
-   Mnemonic: **T**​ag

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key sequence</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t b</td>
<td class="org-left">dtext-insert-tag-b</td>
</tr>


<tr>
<td class="org-left">C-c C-t c</td>
<td class="org-left">dtext-insert-tag-code</td>
</tr>


<tr>
<td class="org-left">C-c C-t i</td>
<td class="org-left">dtext-insert-tag-i</td>
</tr>


<tr>
<td class="org-left">C-c C-t q</td>
<td class="org-left">dtext-insert-tag-quote</td>
</tr>


<tr>
<td class="org-left">C-c C-t s</td>
<td class="org-left">dtext-insert-tag-s</td>
</tr>


<tr>
<td class="org-left">C-c C-t u</td>
<td class="org-left">dtext-insert-tag-u</td>
</tr>
</tbody>
</table>


## Tags for Tables

-   Key-bindings begin with `C-c C-b`
-   Mnemonic: Ta​**b**​le

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key sequence</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-b d</td>
<td class="org-left">dtext-insert-tag-td</td>
</tr>


<tr>
<td class="org-left">C-c C-b h</td>
<td class="org-left">dtext-insert-tag-th</td>
</tr>


<tr>
<td class="org-left">C-c C-b r</td>
<td class="org-left">dtext-insert-tag-tr</td>
</tr>
</tbody>
</table>


## DText-Specific Tags

-   Key-bindings begin with `C-c C-s`
-   Mnemonic: **D**​Text

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key sequence</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t b</td>
<td class="org-left">dtext-insert-tag-b</td>
</tr>


<tr>
<td class="org-left">C-c C-t c</td>
<td class="org-left">dtext-insert-tag-code</td>
</tr>


<tr>
<td class="org-left">C-c C-t i</td>
<td class="org-left">dtext-insert-tag-i</td>
</tr>


<tr>
<td class="org-left">C-c C-t q</td>
<td class="org-left">dtext-insert-tag-quote</td>
</tr>


<tr>
<td class="org-left">C-c C-t s</td>
<td class="org-left">dtext-insert-tag-s</td>
</tr>


<tr>
<td class="org-left">C-c C-t u</td>
<td class="org-left">dtext-insert-tag-u</td>
</tr>
</tbody>
</table>


# dtext-mode.el

A major mode in Emacs for editing Danbooru DText.

DText is the markup language used on Danbooru and other Booru
sites. It implements syntax highlighting and keyboard commands for
inserting basic formatting tags.

To activate this major mode, use `M-x dtext-mode`. To create a
temporary buffer for editing posts, use `M-x dtext-scratch`.


# Install

There are a couple of ways to do it. One way to do it&#x2013;and the easiest&#x2013;is to
download one of the [releases](https://github.com/JohnDevlopment/dtext-mode.el/releases) and install it with `M-x package-install-file`.

Another way to install this is by manually putting `dtext-mode.el` in your
`load-path` and then using `(require 'dtext-mode)`.


# How To Report Bugs

Please use Github Issues to report bugs.


# Supported Tags

Below is a list of DText markup tags and their respective
key-bindings. If you have a selected region, then DText Mode will wrap
the tags around that region.

Each key-binding has three parts: `C-c`, the "group" of said binding,
and a single letter taken from the name of that tag. Each section
below provides a menmonic to help users remember them.


## Common Tags

-   Key-bindings begin with `C-c C-t`
-   Mnemonic: **T**​ag

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key sequence</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t b</td>
<td class="org-left">dtext-insert-tag-b</td>
</tr>


<tr>
<td class="org-left">C-c C-t c</td>
<td class="org-left">dtext-insert-tag-code</td>
</tr>


<tr>
<td class="org-left">C-c C-t i</td>
<td class="org-left">dtext-insert-tag-i</td>
</tr>


<tr>
<td class="org-left">C-c C-t q</td>
<td class="org-left">dtext-insert-tag-quote</td>
</tr>


<tr>
<td class="org-left">C-c C-t s</td>
<td class="org-left">dtext-insert-tag-s</td>
</tr>


<tr>
<td class="org-left">C-c C-t u</td>
<td class="org-left">dtext-insert-tag-u</td>
</tr>
</tbody>
</table>


## Tags for Tables

-   Key-bindings begin with `C-c C-b`
-   Mnemonic: Ta​**b**​le

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key sequence</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-b d</td>
<td class="org-left">dtext-insert-tag-td</td>
</tr>


<tr>
<td class="org-left">C-c C-b h</td>
<td class="org-left">dtext-insert-tag-th</td>
</tr>


<tr>
<td class="org-left">C-c C-b r</td>
<td class="org-left">dtext-insert-tag-tr</td>
</tr>
</tbody>
</table>


## DText-Specific Tags

-   Key-bindings begin with `C-c C-s`
-   Mnemonic: **D**​Text

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key sequence</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t b</td>
<td class="org-left">dtext-insert-tag-b</td>
</tr>


<tr>
<td class="org-left">C-c C-t c</td>
<td class="org-left">dtext-insert-tag-code</td>
</tr>


<tr>
<td class="org-left">C-c C-t i</td>
<td class="org-left">dtext-insert-tag-i</td>
</tr>


<tr>
<td class="org-left">C-c C-t q</td>
<td class="org-left">dtext-insert-tag-quote</td>
</tr>


<tr>
<td class="org-left">C-c C-t s</td>
<td class="org-left">dtext-insert-tag-s</td>
</tr>


<tr>
<td class="org-left">C-c C-t u</td>
<td class="org-left">dtext-insert-tag-u</td>
</tr>
</tbody>
</table>
