---
title: Context menu for column headers of .NET ListView
date: 2008-07-13T16:53:00
slug: context-menu-for-column-headers-of-net-listview
categories: [ "dotNET" ]
---

`ListView` doesn't provide `ColumnHeaderMouseClick` event so I had to find a workaround to display context menu for column headers. There is a [way](http://www.codeproject.com/Articles/23330/Handling-Right-Click-Events-in-ListView-Column-Hea "Handling Right-Click Events in ListView Column Headers") to do this by resorting to Windows API but my solution is easier.

<!--more-->

Let's say we've got a `ListView` control called `contentsListView` and two menus: `generalContextMenu` and `headerContextMenu`. `generalContextMenu` should be displayed when user right-clicks on an item and headerContextMenu should be displayed when he clicks on a column header.

Set the list view's property `ContextMenuStrip` to `generalContextMenu`:

	contentListView.ContextMenuStrip = generalContextMenu;

Add handler for `Opening` event of `generalContextMenu`:

	generalContextMenuStrip.Opening += generalContextMenu_Opening;

I have played with debugger a bit and found out that `contentListView.GetItemAt` returns first visible item of the list even when we actually click on a column header. So, when we get some value this means we either clicked on an item or on a column header. Any list item is located below the header, so if the item `Position.Y` is less than `MousePosition.Y` then user clicked on a column header.

Here is the code for the handler:

```cs
private void generalContextMenu_Opening(object sender, CancelEventArgs e)
{
	Point pt = contentListView.PointToClient(new Point(MousePosition.X, MousePosition.Y));
	ListViewItem item = contentListView.GetItemAt(pt.X, pt.Y);

	// Not an item or a column header
	if (item == null)
	{
		e.Cancel = true;
		return;
	}

	// Is it a column header?
	if (item.Position.Y > pt.Y)
	{
		e.Cancel = true;
		headerContextMenu.Show(MousePosition);
	}
}
```
