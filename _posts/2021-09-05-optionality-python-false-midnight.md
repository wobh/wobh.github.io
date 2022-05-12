---
layout: post
title: "Optionality in Python: False Midnight"
date: "2021-09-05T16:10"
tags: ["optionality", "python"]
---

It was brought to my attention that, for Python2, midnight was also
considered "false".

    >>> from datetime import time
    >>> t0 = time(0)
    >>> t0.isoformat()
    '00:00:00'
    >>> t0.strftime("%I:%M:%S")
    '12:00:00'
    >>> bool(t0)
    False

Python3 has changed this so that all times are truthy. There's a good
article about it here:

"A False Midnight" <https://lwn.net/Articles/590299/>

You know what *is* still `False` in Python? Zero-length duration
instances of the `Timedelta` class:

    >>> import datetime
    >>> p0 = datetime.timedelta(0)
    >>> bool(p0)
    False

<https://docs.python.org/3/library/datetime.html#datetime.timedelta>

Since they are identity elements of datetime addition, we can add
these zero-size timedeltas to our table of identities.

More on that kind of thing later.

