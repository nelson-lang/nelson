%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Fetch and parse an RSS 2.0 or Atom feed into a normalized structure.
%   feed.title   char, the feed/channel title
%   feed.link    char, the feed home page
%   feed.url     char, the requested feed location
%   feed.items   struct array: .title .link .date .summary .image (all char)
% The location may be a http(s) url or a local file path.
%=============================================================================
function feed = newsfeedRead(url, varargin)
  timeout = 30;
  for k = 1:2:numel(varargin)
    if strcmpi(varargin{k}, 'Timeout'); timeout = varargin{k + 1}; end
  end

  url = char(url);
  if isfile(url)
    s = readstruct(url);
  else
    opt = weboptions('ContentType', 'text', 'FollowLocation', true, ...
                     'Timeout', timeout, 'UserAgent', 'Nelson newsfeed/1.0');
    txt = webread(url, opt);
    tf = [tempname(), '.xml'];
    filewrite(tf, txt);
    cleaner = onCleanup(@() removeFileIfExists(tf));
    s = readstruct(tf);
  end

  feed = struct('title', '', 'link', '', 'url', url, 'items', struct([]));
  if isfield(s, 'channel')
    feed = readRss(s, feed);
  elseif isfield(s, 'entry')
    feed = readAtom(s, feed);
  else
    error('Nelson:newsfeed:unsupportedFormat', ...
          _('Unrecognized feed format (neither RSS <channel> nor Atom <entry>).'));
  end
end
%=============================================================================
function feed = readRss(s, feed)
  ch = s.channel;
  feed.title = getText(ch, 'title', '(untitled feed)');
  feed.link = getText(ch, 'link', '');
  if ~isfield(ch, 'item'); return; end
  raw = ch.item;
  n = numel(raw);
  items = repmat(emptyItem(), 1, n);
  for k = 1:n
    it = raw(k);
    items(k).title = getText(it, 'title', '(no title)');
    items(k).link = getText(it, 'link', '');
    items(k).date = firstText(it, {'pubDate', 'date', 'dc_date'}, '');
    items(k).summary = firstText(it, {'description', 'content_encoded', 'summary'}, '');
    items(k).image = getImage(it, items(k).summary);
  end
  feed.items = items;
end
%=============================================================================
function feed = readAtom(s, feed)
  feed.title = getText(s, 'title', '(untitled feed)');
  feed.link = atomLink(s);
  raw = s.entry;
  n = numel(raw);
  items = repmat(emptyItem(), 1, n);
  for k = 1:n
    en = raw(k);
    items(k).title = getText(en, 'title', '(no title)');
    items(k).link = atomLink(en);
    items(k).date = firstText(en, {'updated', 'published'}, '');
    items(k).summary = firstText(en, {'summary', 'content'}, '');
    items(k).image = getImage(en, items(k).summary);
  end
  feed.items = items;
end
%=============================================================================
function url = atomLink(node)
  % Atom <link> carries the url in href; prefer rel="alternate".
  url = '';
  if ~isfield(node, 'link'); return; end
  L = node.link;
  if ischar(L) || isStringScalar(L); url = char(L); return; end
  for k = 1:numel(L)
    one = L(k);
    if isfield(one, 'hrefAttribute')
      href = char(one.hrefAttribute);
      rel = '';
      if isfield(one, 'relAttribute'); rel = char(one.relAttribute); end
      if isempty(url) || strcmpi(rel, 'alternate'); url = href; end
    end
  end
end
%=============================================================================
function url = getImage(node, summary)
  % Representative image: Media RSS thumbnail/content, an image enclosure,
  % or the first <img> found in the summary HTML.
  url = attrUrl(node, 'media_thumbnail');
  if ~isempty(url); return; end
  url = attrUrl(node, 'media_content');
  if ~isempty(url); return; end
  if isfield(node, 'enclosure')
    e = node.enclosure;
    for k = 1:numel(e)
      one = e(k);
      typ = '';
      if isfield(one, 'typeAttribute'); typ = char(one.typeAttribute); end
      if isfield(one, 'urlAttribute') && (isempty(typ) || strncmpi(typ, 'image', 5))
        url = char(one.urlAttribute); return;
      end
    end
  end
  tok = regexp(summary, '<img[^>]+src\s*=\s*["'']([^"'']+)["'']', 'tokens', 'once');
  if ~isempty(tok); url = tok{1}; end
end
%=============================================================================
function url = attrUrl(node, name)
  % Read urlAttribute from a media element; pick the widest when several exist.
  url = '';
  if ~isfield(node, name); return; end
  m = node.(name);
  best = -1;
  for k = 1:numel(m)
    one = m(k);
    if ~isfield(one, 'urlAttribute'); continue; end
    w = 0;
    if isfield(one, 'widthAttribute'); w = str2double(char(one.widthAttribute)); end
    if isnan(w); w = 0; end
    if w > best; best = w; url = char(one.urlAttribute); end
  end
end
%=============================================================================
function v = getText(node, name, default)
  v = default;
  if isfield(node, name)
    v = scalarText(node.(name));
    if isempty(v); v = default; end
  end
end
%=============================================================================
function v = firstText(node, names, default)
  v = default;
  for k = 1:numel(names)
    if isfield(node, names{k})
      c = scalarText(node.(names{k}));
      if ~isempty(c); v = c; return; end
    end
  end
end
%=============================================================================
function c = scalarText(x)
  % Normalize a readstruct field (string / <missing> / attributed element) to
  % a trimmed char row. Elements carrying attributes store their text in 'Text'.
  if isstruct(x)
    if isfield(x, 'Text'); c = scalarText(x(1).Text); else; c = ''; end
    return;
  end
  if isscalar(x) && ismissing(x); c = ''; return; end
  c = strtrim(char(string(x)));
end
%=============================================================================
function it = emptyItem()
  it = struct('title', '', 'link', '', 'date', '', 'summary', '', 'image', '');
end
%=============================================================================
function removeFileIfExists(f)
  if isfile(f); try; rmfile(f); catch; end; end
end
%=============================================================================
