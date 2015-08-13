---
layout: post
title: Windows Vista Credential Providers
date: 2007-07-30 13:31
comments: true
categories: Windows
---

In Windows Vista Microsoft has adopted so called credential providers as a way to log into the system. Your custom [GINA] [1] library [won't work] [2] in Vista so you will have to write a COM component that implements `ICredentialProvider` and `ICredentialProviderCredential` interfaces. I found it pretty easy to write a custom credential provider (the one that works with electronic keys). There are a [good introduction] [3] into the new model, [samples] [4] and [technical reference] [5].

[1]: http://en.wikipedia.org/wiki/Graphical_identification_and_authentication
[2]: http://support.microsoft.com/kb/925520
[3]: http://msdn.microsoft.com/en-us/magazine/cc163489.aspx
[4]: http://www.microsoft.com/en-us/download/details.aspx?id=4057
[5]: http://shellrevealed.com/files/folders/code_samples/entry1019.aspx
