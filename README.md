<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgec8ef76">1. README</a></li>
<li><a href="#org36ac4fd">2. <code>local.el</code></a>
<ul>
<li><a href="#orgf23bcae">2.1. Known Variables</a></li>
</ul>
</li>
</ul>
</div>
</div>

<a id="orgec8ef76"></a>

# README


<a id="org36ac4fd"></a>

# `local.el`

We define a `local.el` to store local links

-   Do not expect `local.el` to exist all the time
-   Do not expect `local.el` to define a consistent set of variables
-   Expect `local.el` to vary wildly in content throughout different machines
-   Basically don't trust `local.el`. It is a necessary evil


<a id="orgf23bcae"></a>

## Known Variables

These variabes have been known to exist at some point

-   **/g-drive-folder:** const
    The path to the local google drive folder, We use this
    mostly so we can sync our org-journal and ledger files
