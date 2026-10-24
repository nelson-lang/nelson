%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function songs = synth_piano_songs()
  % Public-domain pieces for the player piano. Each score is an N-by-3 matrix
  % [start, duration, midi] in units of `unit` seconds, sorted by start, with
  % the preset that suits it. name is the key, label the translated title.
  songs = struct('name', {}, 'label', {}, 'preset', {}, 'unit', {}, 'score', {});
  songs(end + 1) = songEntry('Fur Elise (Beethoven)', _('Fur Elise (Beethoven)'), ...
    'Grand piano', 0.15, furElise());
  songs(end + 1) = songEntry('Ode to Joy (Beethoven)', _('Ode to Joy (Beethoven)'), ...
    'Electric piano', 0.40, odeToJoy());
  songs(end + 1) = songEntry('Korobeiniki (folk)', _('Korobeiniki (folk)'), ...
    'Chiptune', 0.21, korobeiniki());
  songs(end + 1) = songEntry('Canon in D (Pachelbel)', _('Canon in D (Pachelbel)'), ...
    'Crystal bells', 0.30, canonInD());
end
%=============================================================================
function s = songEntry(name, label, preset, unit, score)
  [~, order] = sort(score(:, 1));
  s = struct('name', name, 'label', label, 'preset', preset, 'unit', unit, ...
    'score', score(order, :));
end
%=============================================================================
function score = furElise()
  % Sixteenth notes; the left hand answers the right hand in broken chords.
  E5 = 76; Ds5 = 75; B4 = 71; D5 = 74; C5 = 72; A4 = 69;
  C4 = 60; E4 = 64; Gs4 = 68; A2 = 45; E3 = 52; A3 = 57; E2 = 40; Gs3 = 56;
  turn = [E5 Ds5 E5 B4 D5 C5];
  score = [0 1 E5; 1 1 Ds5];
  score = [score; (2:7)', ones(6, 1), turn'];
  score = [score; brokenBar(8, A4, [A2 E3 A3], [C4 E4 A4])];
  score = [score; brokenBar(14, B4, [E2 E3 Gs3], [E4 Gs4 B4])];
  score = [score; brokenBar(20, C5, [A2 E3 A3], [E4 E5 Ds5])];
  score = [score; (26:31)', ones(6, 1), turn'];
  score = [score; brokenBar(32, A4, [A2 E3 A3], [C4 E4 A4])];
  score = [score; brokenBar(38, B4, [E2 E3 Gs3], [E4 C5 B4])];
  score = [score; 44 4 A4; 44 1 A2; 45 1 E3; 46 2 A3];
end
%=============================================================================
function part = brokenBar(t0, melody, left, right)
  part = [t0 2 melody; ...
    t0 1 left(1); t0 + 1 1 left(2); t0 + 2 1 left(3); ...
    t0 + 3 1 right(1); t0 + 4 1 right(2); t0 + 5 1 right(3)];
end
%=============================================================================
function score = odeToJoy()
  % Quarter notes over a root bass in half notes.
  C4 = 60; D4 = 62; E4 = 64; F4 = 65; G4 = 67;
  phrase = [E4 E4 F4 G4 G4 F4 E4 D4 C4 C4 D4 E4];
  score = [(0:11)', ones(12, 1), phrase'];
  score = [score; 12 1.5 E4; 13.5 0.5 D4; 14 2 D4];
  score = [score; (16:27)', ones(12, 1), phrase'];
  score = [score; 28 1.5 D4; 29.5 0.5 C4; 30 2 C4];
  roots = [48 43 48 43 48 43 48 43];
  for bar = 0:7
    score = [score; 4 * bar 2 roots(bar + 1); 4 * bar + 2 2 roots(bar + 1)];
  end
end
%=============================================================================
function score = korobeiniki()
  % Quarter-note units, melody over an octave-bouncing eighth-note bass.
  E5 = 76; B4 = 71; C5 = 72; D5 = 74; A4 = 69; F5 = 77; A5 = 81; G5 = 79;
  melody = [E5 1; B4 0.5; C5 0.5; D5 1; C5 0.5; B4 0.5; ...
    A4 1; A4 0.5; C5 0.5; E5 1; D5 0.5; C5 0.5; ...
    B4 1.5; C5 0.5; D5 1; E5 1; C5 1; A4 1; A4 2; ...
    0 0.5; D5 1; F5 0.5; A5 1; G5 0.5; F5 0.5; ...
    E5 1.5; C5 0.5; E5 1; D5 0.5; C5 0.5; ...
    B4 1; B4 0.5; C5 0.5; D5 1; E5 1; C5 1; A4 1; A4 1];
  starts = [0; cumsum(melody(1:end - 1, 2))];
  score = [starts, 0.9 * melody(:, 2), melody(:, 1)];
  score = score(score(:, 3) > 0, :);
  roots = [40 45 44 45 38 36 44 45];
  for bar = 0:7
    for k = 0:7
      pitch = roots(bar + 1) + 12 * mod(k, 2);
      score = [score; 4 * bar + k / 2, 0.45, pitch];
    end
  end
end
%=============================================================================
function score = canonInD()
  % The eight-chord ground, each chord as a rising arpeggio over its root.
  roots = [50 45 47 42 43 50 43 45];
  minor = [0 0 1 1 0 0 0 0];
  score = zeros(0, 3);
  for cycle = 0:1
    for c = 1:numel(roots)
      t0 = 8 * (c - 1) + 64 * cycle;
      r = roots(c);
      third = 4 - minor(c);
      arpeggio = r + [12, 12 + 7, 24, 24 + third, 24 + 7, 24 + third, 12 + 7, 12 + third];
      score = [score; t0 8 r; t0 + (0:7)', ones(8, 1), arpeggio'];
    end
  end
end
%=============================================================================
