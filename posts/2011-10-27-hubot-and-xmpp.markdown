---
layout: post
title: Hubot &amp; XMPP
date: 2011-10-27 00:43
comments: true
categories: 
---

Running Hubot on a Jabber server is fairly simple:

1. Install node.js;
2. Install npm;
3. Install redis;
4. Set up environment variables:
	* `export HUBOT_XMPP_USERNAME=[JID]`
	* `export HUBOT_XMPP_PASSWORD=[Password]`
	* `export HUBOT_XMPP_ROOMS=[Room]`
5. Run Hubot: `bin/hubot -a xmpp`
