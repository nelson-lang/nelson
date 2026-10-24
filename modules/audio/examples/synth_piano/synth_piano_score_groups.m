%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function groups = synth_piano_score_groups(score, unit)
  % Split a score [start, duration, midi] (in units of `unit` seconds) into
  % the keystrokes that play it: notes sharing a start and a duration sound
  % together as one chord, hence one buffer and one player. Each group has
  % its start and hold in seconds; unique sorts the groups by start.
  groups = struct('start', {}, 'hold', {}, 'notes', {});
  if isempty(score)
    return
  end
  [keysFound, ~, which] = unique(score(:, 1:2), 'rows');
  for g = 1:size(keysFound, 1)
    groups(end + 1) = struct('start', keysFound(g, 1) * unit, ...
      'hold', max(0.08, 0.95 * keysFound(g, 2) * unit), ...
      'notes', score(which == g, 3)');
  end
end
%=============================================================================
