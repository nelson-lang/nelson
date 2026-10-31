%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A dot-set of a word-cloud property re-lays out the cloud in place (reactive),
% like the reference. Verified headless through the web display list.
%=============================================================================
function c = textCount(f)
  s = jsondecode(__web_display_list__(f));
  p = s.primitives;
  c = 0;
  for k = 1:numel(p)
    if strcmp(p{k}.kind, 'text')
      c = c + 1;
    end
  end
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 900 700]);
h = wordcloud({'alpha', 'beta', 'gamma', 'delta', 'epsilon'}, [9 7 5 3 1], ...
  'MaxDisplayWords', 5);
drawnow();
n5 = textCount(f);
%=============================================================================
% MaxDisplayWords reactive: fewer words are drawn
h.MaxDisplayWords = 2;
drawnow();
n2 = textCount(f);
assert_istrue(n2 < n5);
assert_istrue(n2 >= 2);
%=============================================================================
% Color change re-renders
h.MaxDisplayWords = 5;
drawnow();
dl0 = char(__web_display_list__(f));
h.Color = [0 1 0];
drawnow();
dl1 = char(__web_display_list__(f));
assert_istrue(~isequal(dl0, dl1));
%=============================================================================
% Title change re-renders
dlt0 = char(__web_display_list__(f));
h.Title = 'My Cloud';
drawnow();
dlt1 = char(__web_display_list__(f));
assert_istrue(~isequal(dlt0, dlt1));
%=============================================================================
close(f);
%=============================================================================
