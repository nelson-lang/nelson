%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Returns the list of files (relative paths, '/' separated) that would be
% included in the .nmz archive of the module located at module_path.
%
% options fields (all optional):
%   options.skiptests  logical, when true excludes the root 'tests/' folder.
%
% Selection rules (precedence):
%   1. files-allowlist ('files' array in module.json), when present: only
%      files matching one of its globs are candidates.
%   2. default excludes: always removed (see nmm_pack_default_excludes).
%   3. .nmignore: gitignore-style patterns at module root, ADD excludes
%      ('!' prefix un-ignores, subject to the parent-directory limitation).
%=============================================================================
function files = nmm_pack_select(module_path, options)
  if nargin < 2
    options = struct();
  end
  skiptests = isfield(options, 'skiptests') && options.skiptests;
  if ~endsWith(module_path, '/')
    module_path = [module_path, '/'];
  end
  positives = nmm_pack_default_excludes();
  negatives = {};
  if skiptests
    positives{end + 1} = 'tests/';
  end
  [nmi_pos, nmi_neg] = readNmignore(module_path);
  for k = 1:numel(nmi_pos)
    positives{end + 1} = nmi_pos{k};
  end
  for k = 1:numel(nmi_neg)
    negatives{end + 1} = nmi_neg{k};
  end
  files = walkTree(module_path, '', positives, negatives, {});
  allow = readAllowlist(module_path);
  if ~isempty(allow)
    kept = {};
    for i = 1:numel(files)
      if matchAny(files{i}, allow)
        kept{end + 1} = files{i};
      end
    end
    files = kept;
  end
  if ~isempty(files)
    files = sort(files(:))';
  end
end
%=============================================================================
function files = walkTree(root, rel, positives, negatives, files)
  if isempty(rel)
    listing = dir(root);
  else
    listing = dir([root, rel]);
  end
  for i = 1:numel(listing)
    nm = listing(i).name;
    if strcmp(nm, '.') || strcmp(nm, '..')
      continue;
    end
    if isempty(rel)
      childRel = nm;
    else
      childRel = [rel, '/', nm];
    end
    if listing(i).isdir
      if isExcludedPath(childRel, positives, negatives)
        continue;
      end
      files = walkTree(root, childRel, positives, negatives, files);
    else
      if ~isExcludedPath(childRel, positives, negatives)
        files{end + 1} = childRel;
      end
    end
  end
end
%=============================================================================
function tf = isExcludedPath(relpath, positives, negatives)
  tf = matchAny(relpath, positives);
  if tf
    if matchAny(relpath, negatives)
      tf = false;
    end
  end
end
%=============================================================================
function tf = matchAny(relpath, patterns)
  tf = false;
  for i = 1:numel(patterns)
    if pathMatches(relpath, patterns{i})
      tf = true;
      return;
    end
  end
end
%=============================================================================
function tf = pathMatches(relpath, pat)
  dirOnly = false;
  if endsWith(pat, '/')
    dirOnly = true;
    pat = pat(1:end - 1);
  end
  if isempty(pat)
    tf = false;
    return;
  end
  anchored = any(pat == '/');
  re = globToRegex(pat);
  if anchored
    if dirOnly
      full = ['^', re, '(/.*)?$'];
    else
      full = ['^', re, '$'];
    end
    tf = ~isempty(regexp(relpath, full, 'once'));
  else
    full = ['^', re, '$'];
    segments = regexp(relpath, '/', 'split');
    tf = false;
    for k = 1:numel(segments)
      if ~isempty(regexp(segments{k}, full, 'once'))
        tf = true;
        return;
      end
    end
  end
end
%=============================================================================
function re = globToRegex(pat)
  re = '';
  specials = '.^$+()[]{}|';
  i = 1;
  n = length(pat);
  while i <= n
    c = pat(i);
    if c == '*'
      if i < n && pat(i + 1) == '*'
        if i + 2 <= n && pat(i + 2) == '/'
          re = [re, '(?:.*/)?'];
          i = i + 3;
        else
          re = [re, '.*'];
          i = i + 2;
        end
      else
        re = [re, '[^/]*'];
        i = i + 1;
      end
    elseif c == '?'
      re = [re, '[^/]'];
      i = i + 1;
    elseif any(c == specials)
      re = [re, '\', c];
      i = i + 1;
    else
      re = [re, c];
      i = i + 1;
    end
  end
end
%=============================================================================
function [pos, neg] = readNmignore(module_path)
  pos = {};
  neg = {};
  f = [module_path, '.nmignore'];
  if ~isfile(f)
    return;
  end
  txt = fileread(f);
  lines = regexp(txt, '\r\n|\r|\n', 'split');
  for i = 1:numel(lines)
    ln = strtrim(lines{i});
    if isempty(ln) || startsWith(ln, '#')
      continue;
    end
    if startsWith(ln, '!')
      ln = strtrim(ln(2:end));
      if ~isempty(ln)
        neg{end + 1} = ln;
      end
    else
      pos{end + 1} = ln;
    end
  end
end
%=============================================================================
function allow = readAllowlist(module_path)
  allow = {};
  jf = [module_path, 'module.json'];
  if ~isfile(jf)
    return;
  end
  info = jsondecode(jf, '-file');
  if ~isstruct(info) || ~isfield(info, 'files')
    return;
  end
  value = info.files;
  if isempty(value)
    return;
  end
  [ok, list] = normalizeStringList(value);
  if ~ok
    warning(_('module.json: "files" must be an array of character vectors; entry ignored.'));
    return;
  end
  allow = list;
end
%=============================================================================
function [ok, out] = normalizeStringList(value)
  ok = true;
  out = {};
  if ischar(value)
    out = {value};
  elseif isstring(value)
    value = value(:);
    for i = 1:numel(value)
      out{end + 1} = char(value(i));
    end
  elseif iscell(value)
    for i = 1:numel(value)
      v = value{i};
      if ischar(v)
        out{end + 1} = v;
      elseif isstring(v) && isscalar(v)
        out{end + 1} = char(v);
      else
        ok = false;
        out = {};
        return;
      end
    end
  else
    ok = false;
    return;
  end
  cleaned = {};
  for i = 1:numel(out)
    if ~isempty(out{i})
      cleaned{end + 1} = out{i};
    end
  end
  out = cleaned;
end
%=============================================================================
