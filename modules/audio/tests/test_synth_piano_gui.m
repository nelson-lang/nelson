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
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
% <--RELEASE ONLY-->
%=============================================================================
% Drive the synth_piano example through its own callbacks, as a user would.
%=============================================================================
examplePath = [modulepath('audio'), '/examples/synth_piano'];
previousPath = addpath(examplePath);
cleanupPath = onCleanup(@() path(previousPath));
%=============================================================================
function settleUntilStatus(fig, text, timeout)
  % The player piano runs on the GUI timer: wait for it, not for a fixed time
  % that a loaded machine overruns.
  t0 = tic();
  while ~strcmp(fig.UserData.status.Text, text) && toc(t0) < timeout
    drawnow();
    pause(0.02);
  end
end
%=============================================================================
function settleUntil(condition, timeout)
  % Wait for a state the GUI timer produces, never for a fixed time that a
  % loaded machine overruns.
  t0 = tic();
  while ~condition() && toc(t0) < timeout
    drawnow();
    pause(0.02);
  end
end
%=============================================================================
function h = buttonWithText(fig, text)
  buttons = findall(fig, 'Type', 'uibutton');
  h = [];
  for k = 1:numel(buttons)
    if strcmp(buttons(k).Text, text)
      h = buttons(k);
      return
    end
  end
end
%=============================================================================
function keyDown(fig, character)
  callback = fig.WindowKeyPressFcn;
  callback(fig, struct('Key', character, 'Character', character));
end
%=============================================================================
function keyUp(fig, character)
  callback = fig.WindowKeyReleaseFcn;
  callback(fig, struct('Key', character, 'Character', character));
end
%=============================================================================
function press(fig, character)
  keyDown(fig, character);
  keyUp(fig, character);
end
%=============================================================================
function push(h)
  callback = h.ButtonPushedFcn;
  callback(h, []);
end
%=============================================================================
function skip_if_no_concurrent_audio()
  try
    probe = zeros(1, 1024);
    players = {};
    for streamIndex = 1:5
      players{end + 1} = audioplayer(probe, 44100); %#ok<AGROW>
      play(players{end});
    end
    for streamIndex = 1:numel(players)
      stop(players{streamIndex});
    end
  catch probeError
    skip_testsuite(strcmp(probeError.identifier, 'Nelson:audio:streamFailed'), ...
      probeError.message);
    rethrow(probeError);
  end
end
%=============================================================================
skip_if_no_concurrent_audio();
synth_piano();
fig = findall(0, 'Tag', 'synth_piano');
assert_isequal(numel(fig), 1);
assert_isequal(char(fig.Resize), 'off');
settleUntilStatus(fig, 'Welcome glissando - finished', 20);
app = fig.UserData;
assert_istrue(isvalid(app.timer));
assert_isequal(numel(app.keys), 37);
assert_isequal(app.status.Text, 'Welcome glissando - finished');
%=============================================================================
% AZERTY 'q' plays middle C at octave 4 and lights the C4 key.
press(fig, 'q');
app = fig.UserData;
assert_isequal(app.noteLabel.Text, 'C4');
assert_istrue(app.glow(60 - app.lowMidi + 1) > 0.5);
assert_istrue(numel(app.players) >= 1);
assert_istrue(app.level > 0);
%=============================================================================
% The same keystroke with the same knobs replays the buffer rendered the
% first time; turning a knob renders a new one.
cached = app.renderCache.Count;
assert_istrue(cached >= 1);
press(fig, 'q');
app = fig.UserData;
assert_isequal(app.renderCache.Count, cached);
app.toneKnobs{4}.Value = app.toneKnobs{4}.Value - 1;
press(fig, 'q');
app = fig.UserData;
assert_isequal(app.renderCache.Count, cached + 1);
%=============================================================================
% Chord mode: one key, three notes.
app.chord.Value = 'Major';
press(fig, 'd');
app = fig.UserData;
assert_isequal(app.chordLabel.Text, 'E4  G#4  B4');
app.chord.Value = 'Single';
%=============================================================================
% Mouse glissando: press on a key, drag across the keyboard, release; the
% notes played are read back from a recorded take. The figure pixels of the
% key units are calibrated with two presses outside the keys (below and
% above them), which play nothing.
app = fig.UserData;
app.recButton.Value = true;
recordToggled = app.recButton.ValueChangedFcn;
recordToggled(app.recButton, []);
% Each drawnow runs one queued callback: wait for the state each gesture
% leaves instead.
__figure_mouse_event__(fig, 'press', [100 5]);
low = app.keyAxes.CurrentPoint(1, 1:2);
__figure_mouse_event__(fig, 'press', [1000 240]);
high = app.keyAxes.CurrentPoint(1, 1:2);
__figure_mouse_event__(fig, 'release', [1000 240]);
% those three callbacks leave no trace: drain them
for k = 1:10
  drawnow();
end
assert_istrue(low(2) < 0 && high(2) > 216);
toPixels = @(u)[100, 5] + (u - low) .* ([1000, 240] - [100, 5]) ./ (high - low);
% white keys C4 to G4 (the 8th to 12th white keys from C3), low on the keys
whiteCenters = ((7:11) + 0.5) * 50;
notes = [60 62 64 65 67];
__figure_mouse_event__(fig, 'press', toPixels([whiteCenters(1), 30]));
settleUntil(@() ~ isempty(fig.WindowButtonMotionFcn), 10);
for k = 2:numel(whiteCenters)
  __figure_mouse_event__(fig, 'motion', toPixels([whiteCenters(k), 30]));
  settleUntil(@() fig.UserData.mouseKey == notes(k) - fig.UserData.lowMidi + 1, 10);
end
__figure_mouse_event__(fig, 'release', toPixels([whiteCenters(end), 30]));
settleUntil(@() isempty(fig.WindowButtonMotionFcn), 10);
app = fig.UserData;
assert_isequal(app.noteLabel.Text, 'G4');
assert_isequal(app.take(:, 3)', notes);
assert_isequal(app.mouseKey, 0);
% a black key sits on top of its white neighbours
__figure_mouse_event__(fig, 'press', toPixels([8 * 50, 150]));
__figure_mouse_event__(fig, 'release', toPixels([8 * 50, 150]));
settleUntil(@() size(fig.UserData.take, 1) == 6 && isempty(fig.WindowButtonMotionFcn), 10);
app = fig.UserData;
assert_isequal(app.noteLabel.Text, 'C#4');
assert_isequal(app.take(end, 3), 61);
% A drag faster than the synthesis jumps over keys between two motion
% events: the keys crossed on the way play all the same, in order.
__figure_mouse_event__(fig, 'press', toPixels([whiteCenters(1), 30]));
settleUntil(@() ~ isempty(fig.WindowButtonMotionFcn), 10);
__figure_mouse_event__(fig, 'motion', toPixels([whiteCenters(end), 30]));
settleUntil(@() fig.UserData.mouseKey == notes(end) - fig.UserData.lowMidi + 1, 10);
__figure_mouse_event__(fig, 'release', toPixels([whiteCenters(end), 30]));
settleUntil(@() isempty(fig.WindowButtonMotionFcn), 10);
app = fig.UserData;
assert_isequal(app.take(end - 4:end, 3)', notes);
% at the height of the black keys, they are crossed too
__figure_mouse_event__(fig, 'press', toPixels([whiteCenters(1), 150]));
settleUntil(@() ~ isempty(fig.WindowButtonMotionFcn), 10);
__figure_mouse_event__(fig, 'motion', toPixels([whiteCenters(3), 150]));
settleUntil(@() fig.UserData.mouseKey == 64 - fig.UserData.lowMidi + 1, 10);
__figure_mouse_event__(fig, 'release', toPixels([whiteCenters(3), 150]));
settleUntil(@() isempty(fig.WindowButtonMotionFcn), 10);
app = fig.UserData;
assert_isequal(app.take(end - 4:end, 3)', [60 61 62 63 64]);
app.recButton.Value = false;
recordToggled(app.recButton, []);
%=============================================================================
% QWERTY layout: 'a' is now C, and the key caps follow.
app.layout.Value = 'QWERTY';
callback = app.layout.ValueChangedFcn;
callback(app.layout, []);
press(fig, 'a');
app = fig.UserData;
assert_isequal(app.noteLabel.Text, 'C4');
assert_isequal(app.keyLabels{60 - app.lowMidi + 1}.String, {'A'; 'C4'});
%=============================================================================
% Arrows: octave up, next preset.
press(fig, 'right');
app = fig.UserData;
assert_isequal(app.octave.Value, 5);
press(fig, 'a');
app = fig.UserData;
assert_isequal(app.noteLabel.Text, 'C5');
press(fig, 'up');
app = fig.UserData;
assert_isequal(app.preset.Value, 'Electric piano');
%=============================================================================
% Record a take, replay it, export it. Shift is the sustain pedal: the note
% played while it is down rings for four seconds, the others for the length.
app.recButton.Value = true;
callback = app.recButton.ValueChangedFcn;
callback(app.recButton, []);
keyDown(fig, 'shift');
app = fig.UserData;
assert_istrue(app.sustain);
assert_isequal(app.sustainLamp.Color, app.theme.amber);
press(fig, 'a');
keyUp(fig, 'shift');
app = fig.UserData;
assert_isfalse(app.sustain);
assert_isequal(app.sustainLamp.Color, app.theme.ledOff);
% A held key auto-repeats its press: it plays one note until released.
keyDown(fig, 's');
keyDown(fig, 's');
keyDown(fig, 's');
keyUp(fig, 's');
press(fig, 'd');
app.recButton.Value = false;
callback(app.recButton, []);
app = fig.UserData;
assert_isequal(size(app.take), [3 3]);
assert_isequal(app.take(:, 3)', [72 74 76]);
assert_isequal(app.take(:, 2)', [4, app.length.Value, app.length.Value]);
assert_istrue(issorted(app.take(:, 1)));
push(buttonWithText(fig, 'REPLAY TAKE'));
settleUntilStatus(fig, 'Your take - finished', 20);
app = fig.UserData;
assert_isequal(app.status.Text, 'Your take - finished');
wavFile = fullfile(tempdir(), 'nelson_ns37.wav');
if isfile(wavFile)
  delete(wavFile);
end
push(buttonWithText(fig, 'EXPORT WAV'));
assert_istrue(isfile(wavFile));
[y, fs] = audioread(wavFile);
assert_isequal(fs, 44100);
assert_isequal(size(y, 2), 2);
assert_istrue(size(y, 1) > fs);
assert_istrue(max(abs(y(:))) > 0.05);
delete(wavFile);
%=============================================================================
% The player piano switches to the song's preset and plays it; STOP stops it.
app.song.Value = 'Korobeiniki (folk)';
push(buttonWithText(fig, 'PLAY'));
% every keystroke of the piece is rendered before the first one sounds, so
% nothing is synthesised while it plays
app = fig.UserData;
assert_isequal(app.status.Text, 'Playing: Korobeiniki (folk)');
rendered = app.renderCache.Count;
assert_istrue(rendered >= numel(unique(arrayfun(@(g) mat2str([g.hold, sort(g.notes)]), ...
  app.seq.groups, 'UniformOutput', false))));
settleUntil(@() fig.UserData.seq.index > 10, 20);
app = fig.UserData;
assert_isequal(app.preset.Value, 'Chiptune');
assert_istrue(app.seq.active);
assert_istrue(app.seq.index > 10);
assert_isequal(app.renderCache.Count, rendered);
push(buttonWithText(fig, 'STOP'));
app = fig.UserData;
assert_isfalse(app.seq.active);
assert_isequal(app.status.Text, 'Stopped');
%=============================================================================
% Closing the window stops and releases the timer.
timerHandle = app.timer;
delete(fig);
assert_isfalse(isvalid(timerHandle));
clear cleanupPath;
%=============================================================================
