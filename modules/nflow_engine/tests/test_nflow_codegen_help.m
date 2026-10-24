%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% Every block help page must carry the "Code generation" indicator if and only
% if the block type supports code generation (__nflow_codegen_support__), in both
% en_US and fr_FR. This locks the invariant produced by
% NFlow.internal.generateCodegenHelp so that a new block, or a help page with a layout
% the stamper does not anchor, cannot silently drop (or wrongly gain) the line.
%=============================================================================
hroot = [modulepath('nflow_blocks'), '/help'];
langs = {'en_US', 'fr_FR'};
lineMark = {'Code generation: supported', 'Generation de code'};
skip_testsuite(~isdir([hroot, '/', langs{1}, '/xml']), ...
  'NFlow XML help sources are not present in the installed layout.');
mismatches = {};
nChecked = 0;
for li = 1:numel(langs)
  root = [hroot, '/', langs{li}, '/xml'];
  cats = dir(root);
  for ci = 1:numel(cats)
    if ~cats(ci).isdir || cats(ci).name(1) == '.'
      continue;
    end
    files = dir([root, '/', cats(ci).name, '/*.xml']);
    for fi = 1:numel(files)
      if strcmp(files(fi).name, 'chapter.xml')
        continue;
      end
      f = [root, '/', cats(ci).name, '/', files(fi).name];
      fid = fopen(f, 'rb'); b = fread(fid, inf, 'uint8')'; fclose(fid);
      raw = char(b);
      % Language-neutral / translated block-type label.
      tok = regexp(raw, '(?:Block type|Type de bloc)\s*:\s*([A-Za-z0-9_]+)', 'tokens', 'once');
      if isempty(tok)
        continue;
      end
      type = tok{1};
      [c, r] = __nflow_codegen_support__(type);
      supported = c || r;
      hasLine = ~isempty(strfind(raw, lineMark{li}));
      nChecked = nChecked + 1;
      if supported ~= hasLine
        mismatches{end + 1} = sprintf('%s/%s/%s type=%s supported=%d hasLine=%d', ...
          langs{li}, cats(ci).name, files(fi).name, type, supported, hasLine); %#ok<SAGROW>
      end
    end
  end
end
%=============================================================================
if ~isempty(mismatches)
  msg = sprintf('%s\n', mismatches{:});
  error('nflow codegen help indicator mismatches (run NFlow.internal.generateCodegenHelp):\n%s', msg);
end
% A meaningful number of pages must actually be covered (guard against a broken
% traversal that would make the invariant trivially hold).
assert_istrue(nChecked > 100);
%=============================================================================
