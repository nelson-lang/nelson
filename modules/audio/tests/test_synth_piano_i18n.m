%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The synth_piano example is localizable: its interface texts and error
% templates are picked up by the catalog extraction of the audio module.
%=============================================================================
src = [modulepath('audio'), '/examples/synth_piano'];
previousPath = addpath(src);
cleanupPath = onCleanup(@() path(previousPath));
uiJson = [tempdir(), 'synth_piano_ui.json'];
errorsJson = [tempdir(), 'synth_piano_errors.json'];
i18nHelpers('extractUI', src, uiJson);
i18nHelpers('extractErrors', src, errorsJson);
% raw text: jsondecode would mangle the keys into field names
uiText = fileread(uiJson);
errorsText = fileread(errorsJson);
delete(uiJson);
delete(errorsJson);
%=============================================================================
% panel titles, buttons, knob captions, status templates, preset, chord and
% song names shown in the drop-downs
texts = {'SOUND', 'PLAYER PIANO', 'DISPLAY', 'ENVELOPE', 'TONE & EFFECTS', ...
  'PLAY', 'STOP', 'REPLAY TAKE', 'EXPORT WAV', 'ATTACK', 'RELEASE', 'VOLUME', ...
  'Playing: %s', '%s - finished', 'Preset: %s', 'Take recorded: %d notes', ...
  'Grand piano', 'Chiptune', 'Seventh', 'Fur Elise (Beethoven)', 'Canon in D (Pachelbel)'};
for k = 1:numel(texts)
  assert_istrue(contains(uiText, ['"', texts{k}, '"']), texts{k});
end
%=============================================================================
assert_istrue(contains(errorsText, 'Nelson:synth_piano:noAudioOutput'));
assert_istrue(contains(errorsText, 'Nelson:synth_piano:audioOutputUnusable'));
assert_istrue(contains(errorsText, 'Nelson:synth_piano:unknownPreset'));
%=============================================================================
% the engine keeps its English preset keys, whatever the display language
songs = synth_piano_songs();
assert_isequal({songs.name}, {'Fur Elise (Beethoven)', 'Ode to Joy (Beethoven)', ...
  'Korobeiniki (folk)', 'Canon in D (Pachelbel)'});
assert_isequal(numel({songs.label}), 4);
%=============================================================================
