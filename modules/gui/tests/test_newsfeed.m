%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
asserts.isequal(nargout('newsfeed'), -1);
%=============================================================================
rss_file = [fileparts(nfilename('fullpathext')), '/newsfeed_rss.xml'];
atom_file = [fileparts(nfilename('fullpathext')), '/newsfeed_atom.xml'];
%=============================================================================
% RSS 2.0: title, items, links, media:thumbnail, inline image fallback.
html = newsfeed(rss_file, '-tohtml');
asserts.istrue(ischar(html));
asserts.istrue(contains(html, '<title>Sample RSS Feed</title>'));
asserts.istrue(contains(html, '2 items'));
asserts.istrue(contains(html, 'First article'));
asserts.istrue(contains(html, 'https://example.org/first'));
asserts.istrue(contains(html, 'https://example.org/img/first.jpg'));
asserts.istrue(contains(html, 'https://example.org/inline2.png'));
asserts.istrue(contains(html, 'Mon, 01 Sep 2025 08:00:00 GMT'));
%=============================================================================
% Atom: title at root, entries, link href, content text, media:thumbnail.
html = newsfeed(atom_file, '-tohtml');
asserts.istrue(contains(html, '<h1>Sample Atom Feed</h1>'));
asserts.istrue(contains(html, '2 items'));
asserts.istrue(contains(html, 'Atom entry one'));
asserts.istrue(contains(html, 'https://example.org/atom/1'));
asserts.istrue(contains(html, 'entry one'));
asserts.istrue(contains(html, 'https://example.org/img/atom1.png'));
%=============================================================================
% Unsupported document raises an explicit error.
bad_file = [tempname(), '.xml'];
filewrite(bad_file, '<?xml version="1.0"?><root><a>1</a></root>');
asserts.checkerror(['newsfeed(''', bad_file, ''', ''-tohtml'')'], ...
  _('Unrecognized feed format (neither RSS <channel> nor Atom <entry>).'));
rmfile(bad_file);
%=============================================================================
