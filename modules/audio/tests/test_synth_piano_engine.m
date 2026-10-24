%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sound engine, envelope and scores of the synth_piano example. No audio
% device needed: everything here is plain computation.
%=============================================================================
examplePath = [modulepath('audio'), '/examples/synth_piano'];
previousPath = addpath(examplePath);
cleanupPath = onCleanup(@() path(previousPath));
%=============================================================================
fs = 44100;
params = struct('preset', 'Grand piano', 'fs', fs, 'attack', 0.01, 'decay', 1, ...
  'sustain', 0.5, 'release', 0.5, 'hold', 0.6, 'brightness', 0.6, ...
  'echo', 0, 'space', 0, 'volume', 0.7);
%=============================================================================
% Every preset renders a finite, bounded stereo buffer of the expected length.
presets = {'Grand piano', 'Electric piano', 'Super saw', 'Drawbar organ', ...
  'Crystal bells', 'Chiptune'};
expectedLength = round((params.attack + params.hold + params.release) * fs);
for k = 1:numel(presets)
  params.preset = presets{k};
  y = synth_piano_engine([60 64 67], params);
  assert_isequal(size(y), [expectedLength, 2]);
  assert_istrue(all(isfinite(y(:))));
  assert_istrue(max(abs(y(:))) <= 1);
  assert_istrue(max(abs(y(:))) > 0.01);
end
%=============================================================================
% The pitch is right: an A4 bell, whose fundamental dominates, peaks at 440 Hz.
params.preset = 'Crystal bells';
y = synth_piano_engine(69, params);
n = 16384;
magnitude = abs(fft(y(1:n, 1)));
[~, peak] = max(magnitude(1:n / 2));
assert_istrue(abs((peak - 1) * fs / n - 440) < 2 * fs / n);
%=============================================================================
% Low notes sit on the left, high notes on the right.
params.preset = 'Drawbar organ';
low = synth_piano_engine(36, params);
high = synth_piano_engine(96, params);
assert_istrue(sum(low(:, 1) .^ 2) > 10 * sum(low(:, 2) .^ 2));
assert_istrue(sum(high(:, 2) .^ 2) > 10 * sum(high(:, 1) .^ 2));
%=============================================================================
% Echo and space lengthen the buffer by their tails, and end in silence.
params.echo = 0.5;
params.space = 0.5;
y = synth_piano_engine(60, params);
assert_istrue(size(y, 1) > expectedLength + round(0.9 * fs));
assert_istrue(abs(y(end, 1)) < 1e-9 && abs(y(end, 2)) < 1e-9);
params.echo = 0;
params.space = 0;
%=============================================================================
assert_checkerror('synth_piano_engine(60, setfield(params, ''preset'', ''Theremin''))', ...
  'Unknown preset: Theremin', 'Nelson:synth_piano:unknownPreset');
%=============================================================================
% Envelope: starts at zero, reaches one after the attack, holds the sustain
% level, and has decayed well below it at the end of the release.
t = (0:round(3 * fs) - 1)' / fs;
env = synth_piano_envelope(t, struct('attack', 0.1, 'decay', 0.4, 'sustain', 0.6, ...
  'release', 0.5, 'hold', 1));
assert_isequal(env(1), 0);
assert_istrue(abs(env(round(0.1 * fs)) - 1) < 1e-3);
assert_istrue(abs(env(round(1.0 * fs)) - 0.6) < 1e-3);
assert_istrue(env(round(1.6 * fs)) < 0.01);
assert_istrue(all(env >= 0 & env <= 1));
%=============================================================================
% Scores: sorted [start, duration, midi] rows on the piano range, presets known.
songs = synth_piano_songs();
assert_istrue(numel(songs) >= 4);
for k = 1:numel(songs)
  score = songs(k).score;
  assert_isequal(size(score, 2), 3);
  assert_istrue(issorted(score(:, 1)));
  assert_istrue(all(score(:, 2) > 0));
  assert_istrue(all(score(:, 3) >= 21 & score(:, 3) <= 108));
  assert_istrue(all(score(:, 3) == round(score(:, 3))));
  assert_istrue(any(strcmp(songs(k).preset, presets)));
  assert_istrue(songs(k).unit > 0);
end
%=============================================================================
% Keys under the mouse: C, C#, D, E with a 2-unit gap between white keys;
% the black key covers the upper part of its white neighbours.
keyBox = [0 0 48 216; 34 78 32 138; 50 0 48 216; 100 0 48 216];
keyBlack = [false true false false];
assert_isequal(synth_piano_keys_at(keyBox, keyBlack, [10 30], [10 30]), 1);
assert_isequal(synth_piano_keys_at(keyBox, keyBlack, [40 150], [40 150]), 2);
assert_isequal(synth_piano_keys_at(keyBox, keyBlack, [40 30], [40 30]), 1);
assert_istrue(isempty(synth_piano_keys_at(keyBox, keyBlack, [49 30], [49 30])));
assert_istrue(isempty(synth_piano_keys_at(keyBox, keyBlack, [200 30], [200 30])));
% a fast drag crosses several keys between two events: all of them, in order
assert_isequal(synth_piano_keys_at(keyBox, keyBlack, [10 30], [120 30]), [1 3 4]);
assert_isequal(synth_piano_keys_at(keyBox, keyBlack, [120 30], [10 30]), [4 3 1]);
assert_isequal(synth_piano_keys_at(keyBox, keyBlack, [10 150], [120 150]), [1 2 3 4]);
%=============================================================================
% Keystrokes of a score: notes sharing a start and a duration are one chord,
% groups come sorted by start, times in seconds, holds never below 80 ms.
score = [2 1 64; 0 2 60; 0 2 67; 0 1 72; 1 0.01 62];
groups = synth_piano_score_groups(score, 0.5);
assert_isequal(numel(groups), 4);
assert_isequal([groups.start], [0 0 0.5 1]);
assert_isequal(groups(1).notes, 72);
assert_isequal(sort(groups(2).notes), [60 67]);
assert_isequal([groups.hold], [0.475 0.95 0.08 0.475]);
assert_istrue(isempty(synth_piano_score_groups(zeros(0, 3), 1)));
% every song is rendered before it plays: its distinct keystrokes must fit
% the render cache of the example (96 buffers)
for k = 1:numel(songs)
  groups = synth_piano_score_groups(songs(k).score, songs(k).unit);
  signatures = arrayfun(@(g) mat2str([g.hold, sort(g.notes)]), groups, 'UniformOutput', false);
  assert_istrue(numel(unique(signatures)) < 96, songs(k).name);
end
%=============================================================================
clear cleanupPath;
%=============================================================================
