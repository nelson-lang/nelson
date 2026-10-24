%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function synth_piano()
  % NS-37: a three-octave polyphonic synthesizer written in pure Nelson.
  %
  % Every keystroke is synthesised on the spot (additive piano strings, FM
  % electric piano, super saw, drawbar organ, bells, 8-bit pulse), shaped by
  % the ADSR knobs, spread in stereo, echoed, and handed to an audioplayer.
  % Several players sound at once, which is the polyphony. One GUI timer
  % drives everything that moves: the keys fading back from their neon
  % colour, the falling spectrum bars, the VU needle, the voice LEDs and the
  % player piano that performs the demo songs.
  %
  % Play with the mouse (drag across the keys for a glissando), or with the
  % computer keyboard:
  %   AZERTY  q s d f g h j k l m  (white)   z e t y u o p  (black)
  %   QWERTY  a s d f g h j k l ;  (white)   w e t y u o p  (black)
  %   Left/Right arrows: octave, Up/Down arrows: preset, Space: play/stop,
  %   Shift held: sustain pedal.
  devices = audiodevinfo();
  if isempty(devices.output)
    error('Nelson:synth_piano:noAudioOutput', 'This demo requires an audio output device.');
  end
  addpath([modulepath('audio'), '/examples/synth_piano']);
  % A device can be listed and still unusable (no active sound output in the
  % system): find out now, with a message that says what to check.
  fs = 44100;
  try
    synthWarmUp(fs);
  catch err
    error('Nelson:synth_piano:audioOutputUnusable', ...
      'No usable audio output (%s): check the sound output settings of the system.', err.message);
  end
  existing = findall(0, 'Tag', 'synth_piano');
  if ~isempty(existing)
    delete(existing);
  end
  app = struct();
  app.fs = fs;
  app.theme = synthTheme();
  app.presets = synthPresetTable();
  app.songs = synth_piano_songs();
  app.clock = tic();
  fig = uifigure('Name', _('Nelson NS-37 synthesizer'), 'Tag', 'synth_piano', 'Resize', 'off', ...
    'Position', synthCenteredPosition(1180, 745), 'Color', app.theme.background);
  app = synthBuildHeader(fig, app);
  app = synthBuildSoundPanel(fig, app);
  app = synthBuildPlayerPanel(fig, app);
  app = synthBuildDisplay(fig, app);
  app = synthBuildEnvelopePanel(fig, app);
  app = synthBuildTonePanel(fig, app);
  app = synthBuildKeyboard(fig, app);
  app.players = {};
  app.level = 0;
  app.spectrum = zeros(1, 64);
  app.heldKeys = {};
  app.seq = struct('active', false, 'groups', synth_piano_score_groups([], 1), ...
    'index', 1, 't0', 0, 'name', '');
  app.take = zeros(0, 3);
  app.recording = false;
  app.recT0 = 0;
  app.sustain = false;
  app.renderCache = containers.Map();
  app.cacheLimit = 96;
  app.timer = timer('Period', 0.04, 'ExecutionMode', 'fixedSpacing', ...
    'BusyMode', 'drop', 'TimerFcn', @(src, evt) synthTick(src, fig));
  fig.UserData = app;
  synthApplyPreset(fig, app.presets(1).name);
  synthRelabelKeys(fig);
  fig.WindowKeyPressFcn = @(src, evt) synthKeyPressed(fig, evt);
  fig.WindowKeyReleaseFcn = @(src, evt) synthKeyReleased(fig, evt);
  fig.WindowButtonDownFcn = @(src, evt) synthMousePressed(fig);
  fig.WindowButtonUpFcn = @(src, evt) synthMouseReleased(fig);
  fig.DeleteFcn = @(src, evt) synthShutdown(fig);
  start(app.timer);
  synthStartScore(fig, synthIntroScore(), 0.035, _('Welcome glissando'));
end
%=============================================================================
function theme = synthTheme()
  theme = struct();
  theme.background = [0.035 0.043 0.075];
  theme.panel = [0.063 0.075 0.118];
  theme.screen = [0.020 0.027 0.050];
  theme.text = [0.80 0.85 0.95];
  theme.dim = [0.45 0.51 0.64];
  theme.cyan = [0.25 0.92 1.00];
  theme.magenta = [1.00 0.32 0.78];
  theme.amber = [1.00 0.72 0.25];
  theme.white = [0.94 0.95 0.98];
  theme.black = [0.10 0.11 0.15];
  theme.ledOff = [0.13 0.15 0.22];
end
%=============================================================================
function presets = synthPresetTable()
  % Knob positions (0..100) for attack, decay, sustain, release, tone, and the
  % note length in seconds. name is the key the engine knows, label the
  % translated text shown.
  names = {'Grand piano', 'Electric piano', 'Super saw', 'Drawbar organ', ...
    'Crystal bells', 'Chiptune'};
  labels = {_('Grand piano'), _('Electric piano'), _('Super saw'), _('Drawbar organ'), ...
    _('Crystal bells'), _('Chiptune')};
  knobs = [3 60 55 45 60 1.0; 3 50 60 45 50 1.0; 20 40 80 55 65 0.8; ...
    5 20 100 20 60 0.8; 2 70 30 70 55 1.2; 2 30 70 15 40 0.4];
  presets = struct('name', names, 'label', labels, 'knobs', num2cell(knobs, 2)');
end
%=============================================================================
function position = synthCenteredPosition(width, height)
  screen = get(0, 'ScreenSize');
  left = screen(1) + max(10, (screen(3) - width) / 2);
  bottom = screen(2) + max(40, (screen(4) - height) / 2);
  position = [left bottom width height];
end
%=============================================================================
function h = synthLabel(parent, text, position, color, fontSize, weight)
  h = uilabel(parent, 'Text', text, 'Position', position, 'FontColor', color, ...
    'FontSize', fontSize, 'FontWeight', weight);
end
%=============================================================================
function p = synthPanel(parent, app, title, position)
  p = uipanel(parent, 'Title', title, 'Position', position, ...
    'BackgroundColor', app.theme.panel, 'ForegroundColor', app.theme.cyan, ...
    'BorderColor', [0.16 0.20 0.32], 'FontWeight', 'bold', 'FontSize', 11);
end
%=============================================================================
function app = synthBuildHeader(fig, app)
  th = app.theme;
  synthLabel(fig, 'NELSON  NS-37', [20 700 260 36], th.cyan, 26, 'bold');
  synthLabel(fig, _('POLYPHONIC SYNTHESIZER'), [285 704 330 26], th.magenta, 16, 'bold');
  synthLabel(fig, _('Every note is synthesised live in pure Nelson - play with the mouse or the computer keyboard'), ...
    [22 676 700 20], th.dim, 12, 'normal');
  app.status = synthLabel(fig, _('Ready'), [640 690 520 24], th.amber, 12, 'normal');
  app.status.HorizontalAlignment = 'right';
end
%=============================================================================
function app = synthBuildSoundPanel(fig, app)
  th = app.theme;
  p = synthPanel(fig, app, _('SOUND'), [15 455 245 210]);
  rows = [155 120 85 50 15];
  names = {_('Preset'), _('Octave'), _('Chord'), _('Keys'), _('Length (s)')};
  for k = 1:numel(rows)
    synthLabel(p, names{k}, [10 rows(k) 85 24], th.text, 12, 'normal');
  end
  % Translated Items, English ItemsData: Value stays the key the code uses.
  app.preset = uidropdown(p, 'Items', {app.presets.label}, 'ItemsData', {app.presets.name}, ...
    'Position', [95 rows(1) 138 24], 'ValueChangedFcn', @(src, evt) synthPresetChanged(fig));
  app.octave = uispinner(p, 'Limits', [1 6], 'Value', 4, 'Step', 1, ...
    'Position', [95 rows(2) 138 24], 'ValueChangedFcn', @(src, evt) synthRelabelKeys(fig));
  app.chord = uidropdown(p, 'Items', {_('Single'), _('Major'), _('Minor'), _('Seventh'), ...
    _('Power'), _('Octaves')}, 'ItemsData', {'Single', 'Major', 'Minor', 'Seventh', 'Power', 'Octaves'}, ...
    'Position', [95 rows(3) 138 24]);
  app.layout = uidropdown(p, 'Items', {'AZERTY', 'QWERTY'}, 'Position', [95 rows(4) 138 24], ...
    'ValueChangedFcn', @(src, evt) synthRelabelKeys(fig));
  app.length = uispinner(p, 'Limits', [0.1 4], 'Value', 1, 'Step', 0.1, ...
    'Position', [95 rows(5) 138 24], 'ValueChangedFcn', @(src, evt) synthDrawEnvelope(fig));
end
%=============================================================================
function app = synthBuildPlayerPanel(fig, app)
  th = app.theme;
  p = synthPanel(fig, app, _('PLAYER PIANO'), [15 250 245 195]);
  app.song = uidropdown(p, 'Items', {app.songs.label}, 'ItemsData', {app.songs.name}, ...
    'Position', [10 135 223 26]);
  app.playButton = uibutton(p, 'Text', _('PLAY'), 'Position', [10 95 108 30], ...
    'BackgroundColor', [0.10 0.45 0.35], 'FontColor', [1 1 1], 'FontWeight', 'bold', ...
    'ButtonPushedFcn', @(src, evt) synthPlaySong(fig));
  uibutton(p, 'Text', _('STOP'), 'Position', [125 95 108 30], ...
    'BackgroundColor', [0.30 0.14 0.20], 'FontColor', [1 1 1], 'FontWeight', 'bold', ...
    'ButtonPushedFcn', @(src, evt) synthStopScore(fig, _('Stopped')));
  app.recButton = uibutton(p, 'state', 'Text', _('REC'), 'Position', [10 52 70 30], ...
    'FontWeight', 'bold', 'ValueChangedFcn', @(src, evt) synthRecordToggled(fig));
  app.recLamp = uilamp(p, 'Position', [86 58 18 18], 'Color', th.ledOff);
  uibutton(p, 'Text', _('REPLAY TAKE'), 'Position', [112 52 121 30], ...
    'ButtonPushedFcn', @(src, evt) synthReplayTake(fig));
  uibutton(p, 'Text', _('EXPORT WAV'), 'Position', [10 12 223 30], ...
    'ButtonPushedFcn', @(src, evt) synthExport(fig));
end
%=============================================================================
function app = synthBuildDisplay(fig, app)
  th = app.theme;
  p = synthPanel(fig, app, _('DISPLAY'), [270 250 480 415]);
  app.noteLabel = synthLabel(p, '--', [15 335 130 52], th.cyan, 38, 'bold');
  app.freqLabel = synthLabel(p, _('press a key'), [15 310 300 22], th.text, 13, 'normal');
  app.chordLabel = synthLabel(p, '', [15 290 205 20], th.dim, 12, 'normal');
  app.sustainLamp = uilamp(p, 'Position', [228 292 15 15], 'Color', th.ledOff);
  synthLabel(p, _('SUSTAIN'), [248 290 70 18], th.dim, 11, 'bold');
  synthLabel(p, _('VOICES'), [150 365 120 18], th.dim, 11, 'bold');
  app.lamps = cell(1, 8);
  for k = 1:8
    app.lamps{k} = uilamp(p, 'Position', [150 + 20 * (k - 1) 343 15 15], 'Color', th.ledOff);
  end
  app.gauge = uigauge(p, 'semicircular', 'Limits', [0 100], 'Value', 0, ...
    'Position', [318 285 150 100], 'FontColor', th.text, 'BackgroundColor', th.panel, ...
    'ScaleColors', [0.20 0.85 0.45; 1.00 0.75 0.20; 1.00 0.25 0.30], ...
    'ScaleColorLimits', [0 70; 70 90; 90 100]);
  level = synthLabel(p, _('LEVEL'), [343 270 120 16], th.dim, 10, 'bold');
  level.HorizontalAlignment = 'center';
  synthLabel(p, _('OSCILLOSCOPE'), [12 262 200 18], th.dim, 10, 'bold');
  app.scopeAxes = synthScreenAxes(p, th, [10 145 458 115], [0 1], [-1.15 1.15]);
  x = linspace(0, 1, 600);
  app.scopeGlow = plot(app.scopeAxes, x, zeros(size(x)), 'Color', [0.08 0.35 0.45], 'LineWidth', 6);
  app.scopeLine = plot(app.scopeAxes, x, zeros(size(x)), 'Color', th.cyan, 'LineWidth', 1.6);
  synthLabel(p, _('SPECTRUM  40 Hz - 12 kHz'), [12 122 250 18], th.dim, 10, 'bold');
  app.specAxes = synthScreenAxes(p, th, [10 8 458 112], [0 65], [0 1.08]);
  [bx, by] = synthBars(zeros(1, 64));
  app.specBars = plot(app.specAxes, bx, by, 'Color', th.magenta, 'LineWidth', 5);
  app.specPeaks = plot(app.specAxes, 1:64, zeros(1, 64), 'LineStyle', 'none', ...
    'Marker', 's', 'MarkerSize', 3, 'MarkerFaceColor', [1 0.85 0.95], 'Color', [1 0.85 0.95]);
  app.peaks = zeros(1, 64);
end
%=============================================================================
function ax = synthScreenAxes(parent, th, position, xl, yl)
  ax = uiaxes(parent, 'Position', position);
  set(ax, 'Color', th.screen, 'XColor', [0.16 0.20 0.32], 'YColor', [0.16 0.20 0.32], ...
    'XLim', xl, 'YLim', yl, 'XTick', [], 'YTick', [], 'Box', 'on');
  hold(ax, 'on');
end
%=============================================================================
function [bx, by] = synthBars(values)
  % One line object for all the bars: vertical segments separated by NaN.
  n = numel(values);
  bx = reshape([1:n; 1:n; NaN(1, n)], 1, []);
  by = reshape([zeros(1, n); values(:)'; NaN(1, n)], 1, []);
end
%=============================================================================
function app = synthBuildEnvelopePanel(fig, app)
  th = app.theme;
  p = synthPanel(fig, app, _('ENVELOPE'), [760 430 405 235]);
  app.adsrAxes = synthScreenAxes(p, th, [10 130 383 77], [0 1], [0 1.1]);
  app.adsrFill = patch(app.adsrAxes, [0 1 1 0], [0 0 0 0], th.cyan, ...
    'FaceAlpha', 0.18, 'EdgeColor', 'none');
  app.adsrGlow = plot(app.adsrAxes, [0 1], [0 0], 'Color', [0.08 0.35 0.45], 'LineWidth', 5);
  app.adsrLine = plot(app.adsrAxes, [0 1], [0 0], 'Color', th.cyan, 'LineWidth', 1.8);
  names = {_('ATTACK'), _('DECAY'), _('SUSTAIN'), _('RELEASE')};
  app.envKnobs = cell(1, 4);
  for k = 1:4
    app.envKnobs{k} = synthKnob(p, synthKnobCenter(k), 68, ...
      @(src, evt) synthDrawEnvelope(fig, k, evt.Value));
    synthKnobCaption(p, th, names{k}, synthKnobCenter(k), 4);
  end
end
%=============================================================================
function app = synthBuildTonePanel(fig, app)
  th = app.theme;
  p = synthPanel(fig, app, _('TONE & EFFECTS'), [760 250 405 170]);
  names = {_('TONE'), _('ECHO'), _('SPACE'), _('VOLUME')};
  values = [60 25 40 70];
  app.toneKnobs = cell(1, 4);
  for k = 1:4
    app.toneKnobs{k} = synthKnob(p, synthKnobCenter(k), 80, []);
    app.toneKnobs{k}.Value = values(k);
    synthKnobCaption(p, th, names{k}, synthKnobCenter(k), 20);
  end
  synthLabel(p, _('arrows: octave / preset   space: play   shift: sustain'), [10 2 385 16], th.dim, 10, 'normal');
end
%=============================================================================
function x = synthKnobCenter(k)
  % Knobs 100 px apart: the "100" of one dial clears the "0" of the next.
  x = 52 + 100 * (k - 1);
end
%=============================================================================
function synthKnobCaption(parent, th, text, x, y)
  caption = synthLabel(parent, text, [x - 49, y, 98, 18], th.text, 10, 'bold');
  caption.HorizontalAlignment = 'center';
end
%=============================================================================
function k = synthKnob(parent, x, y, changing)
  % A 50 px dial centred on (x, y), numbered at both ends and in the middle
  % with a graduation every tenth of the travel.
  k = uiknob(parent, 'Limits', [0 100], 'Position', [x - 25, y - 25, 50, 50], ...
    'FontColor', [0.55 0.60 0.72], 'MajorTicks', [0 50 100], 'MinorTicks', 0:10:100);
  if ~isempty(changing)
    k.ValueChangingFcn = changing;
    k.ValueChangedFcn = changing;
  end
end
%=============================================================================
function app = synthBuildKeyboard(fig, app)
  % Three octaves from C3, drawn in an axes so that the mouse can glide over
  % them: the pointer is followed through CurrentPoint, in the key units
  % below (one white key = 50, the keys 216 high). White keys first so the
  % black keys sit on top.
  th = app.theme;
  app.lowMidi = 48;
  midi = app.lowMidi + (0:36);
  isBlack = ismember(mod(midi, 12), [1 3 6 8 10]);
  whiteWidth = 50;
  width = sum(~isBlack) * whiteWidth;
  x0 = (1180 - width) / 2;
  % The panel paints the keys in the window's own pixels; the axes margins
  % (left 11, right 4, bottom 17, top 18) keep the drawing on the key area.
  board = uipanel(fig, 'Position', [0 0 1180 246], 'BorderType', 'none', ...
    'BackgroundColor', th.background);
  app.keyAxes = uiaxes(board, 'Position', [x0 - 11, -3, width + 15, 251]);
  set(app.keyAxes, 'XLim', [0 width], 'YLim', [0 216], 'Color', th.background);
  axis(app.keyAxes, 'off');
  hold(app.keyAxes, 'on');
  app.keys = cell(1, numel(midi));
  app.keyLabels = cell(1, numel(midi));
  app.keyBlack = isBlack;
  app.keyBox = zeros(numel(midi), 4);
  app.keyBase = zeros(numel(midi), 3);
  app.keyNeon = zeros(numel(midi), 3);
  app.glow = zeros(1, numel(midi));
  order = [find(~isBlack), find(isBlack)];
  for k = order
    whitesBefore = sum(~isBlack(1:k - 1));
    if isBlack(k)
      box = [whitesBefore * whiteWidth - 16, 78, 32, 138];
      base = th.black;
      fontColor = [0.70 0.75 0.85];
    else
      box = [whitesBefore * whiteWidth, 0, whiteWidth - 2, 216];
      base = th.white;
      fontColor = [0.30 0.34 0.45];
    end
    app.keyBox(k, :) = box;
    app.keyBase(k, :) = base;
    app.keyNeon(k, :) = synthHue((k - 1) / numel(midi) * 0.85);
    xs = box(1) + [0, box(3), box(3), 0];
    ys = box(2) + [0, 0, box(4), box(4)];
    app.keys{k} = patch(app.keyAxes, xs, ys, base, 'EdgeColor', th.background);
    app.keyLabels{k} = text(app.keyAxes, box(1) + box(3) / 2, box(2) + 6, '', ...
      'Color', fontColor, 'FontWeight', 'bold', 'FontSize', 11, ...
      'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom');
  end
  app.mouseKey = 0;
  app.mousePoint = [0 0];
end
%=============================================================================
function synthMousePressed(fig)
  % Press on a key plays it; dragging then plays every key the pointer
  % crosses: a glissando. Motion is only listened to while the button is down.
  app = fig.UserData;
  cp = app.keyAxes.CurrentPoint;
  k = synth_piano_keys_at(app.keyBox, app.keyBlack, cp(1, 1:2), cp(1, 1:2));
  if isempty(k)
    return
  end
  app.mouseKey = k;
  app.mousePoint = cp(1, 1:2);
  fig.UserData = app;
  fig.WindowButtonMotionFcn = @(src, evt) synthMouseMoved(fig);
  synthUserNotes(fig, app.lowMidi + k - 1);
end
%=============================================================================
function synthMouseMoved(fig)
  % Every key crossed since the last event plays, in order, even when the
  % drag outruns the synthesis.
  app = fig.UserData;
  cp = app.keyAxes.CurrentPoint;
  keys = synth_piano_keys_at(app.keyBox, app.keyBlack, app.mousePoint, cp(1, 1:2));
  if ~isempty(keys) && keys(1) == app.mouseKey
    keys(1) = [];
  end
  app.mousePoint = cp(1, 1:2);
  if ~isempty(keys)
    app.mouseKey = keys(end);
  end
  fig.UserData = app;
  for k = keys
    synthUserNotes(fig, app.lowMidi + k - 1);
  end
end
%=============================================================================
function synthMouseReleased(fig)
  fig.WindowButtonMotionFcn = [];
  app = fig.UserData;
  app.mouseKey = 0;
  fig.UserData = app;
end
%=============================================================================
function rgb = synthHue(h)
  % Saturated neon colour for a hue in [0, 1).
  k = mod([5 3 1] + 6 * h, 6);
  rgb = 1 - 0.85 * max(0, min(1, min(k, 4 - k)));
end
%=============================================================================
function [chars, offsets] = synthKeyMap(layout)
  if strcmp(layout, 'QWERTY')
    chars = 'awsedftgyhujkolp;';
  else
    chars = 'qzsedftgyhujkolpm';
  end
  offsets = 0:16;
end
%=============================================================================
function synthRelabelKeys(fig)
  % Show which computer key plays which piano key; C keys carry their name.
  app = fig.UserData;
  [chars, offsets] = synthKeyMap(app.layout.Value);
  base = 12 * (app.octave.Value + 1);
  for k = 1:numel(app.keys)
    midi = app.lowMidi + k - 1;
    hit = find(base + offsets == midi, 1);
    label = '';
    if ~isempty(hit)
      label = upper(chars(hit));
    end
    if mod(midi, 12) == 0
      label = {label; synthNoteName(midi)};
    end
    app.keyLabels{k}.String = label;
  end
end
%=============================================================================
function name = synthNoteName(midi)
  names = {'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'};
  name = sprintf('%s%d', names{mod(midi, 12) + 1}, floor(midi / 12) - 1);
end
%=============================================================================
function params = synthParams(app, hold, env)
  % Knob positions to physical values. Times grow with the square of the
  % knob so the short, musically important end gets most of the travel.
  % env, when given, replaces the envelope knob positions (a knob being
  % dragged reports its value before it owns it).
  if nargin < 3
    env = cellfun(@(k) k.Value, app.envKnobs);
  end
  env = env / 100;
  tone = cellfun(@(k) k.Value, app.toneKnobs) / 100;
  params = struct('preset', app.preset.Value, 'fs', app.fs, ...
    'attack', 1.5 * env(1) ^ 2, 'decay', 0.05 + 3 * env(2) ^ 2, ...
    'sustain', env(3), 'release', 0.05 + 3 * env(4) ^ 2, 'hold', hold, ...
    'brightness', tone(1), 'echo', tone(2), 'space', tone(3), 'volume', tone(4));
end
%=============================================================================
function synthUserNotes(fig, root)
  % A key played by the user: expand the chord, remember it for the take.
  app = fig.UserData;
  intervals = synthChordIntervals(app.chord.Value);
  notes = root + intervals;
  % The pedal holds the strings: a sustained note rings for at least four
  % seconds instead of the note length.
  hold = app.length.Value;
  if app.sustain
    hold = max(4, hold);
  end
  if app.recording
    elapsed = toc(app.clock) - app.recT0;
    app.take = [app.take; repmat([elapsed hold], numel(notes), 1), notes(:)];
    fig.UserData = app;
  end
  synthPlayNotes(fig, notes, hold);
end
%=============================================================================
function intervals = synthChordIntervals(name)
  switch name
    case 'Major'
      intervals = [0 4 7];
    case 'Minor'
      intervals = [0 3 7];
    case 'Seventh'
      intervals = [0 4 7 10];
    case 'Power'
      intervals = [0 7 12];
    case 'Octaves'
      intervals = [0 12];
    otherwise
      intervals = 0;
  end
end
%=============================================================================
function synthPlayNotes(fig, notes, hold)
  % Synthesise, start the sound first (latency is what the ear judges), then
  % light the keys and refresh the screens.
  app = fig.UserData;
  y = synthRender(app, notes, synthParams(app, hold));
  player = audioplayer(y, app.fs);
  play(player);
  app.players{end + 1} = player;
  inRange = notes(notes >= app.lowMidi & notes < app.lowMidi + numel(app.keys));
  app.glow(inRange - app.lowMidi + 1) = 1;
  for m = inRange
    k = m - app.lowMidi + 1;
    app.keys{k}.FaceColor = app.keyNeon(k, :);
  end
  app.level = max(app.level, max(abs(y(:))));
  app = synthShowSignal(app, y, notes);
  fig.UserData = app;
end
%=============================================================================
function y = synthRender(app, notes, params)
  % A keystroke is a pure function of its notes and of the knob positions:
  % a repeated note, a held rhythm or a song replays the buffer rendered the
  % first time instead of synthesising it again.
  key = [params.preset, sprintf('|%.17g', params.fs, params.attack, params.decay, ...
    params.sustain, params.release, params.hold, params.brightness, params.echo, ...
    params.space, params.volume, notes)];
  cache = app.renderCache;
  if isKey(cache, key)
    y = cache(key);
    return
  end
  y = synth_piano_engine(notes, params);
  % A few seconds of stereo audio each: bound the memory held.
  if cache.Count >= app.cacheLimit
    remove(cache, keys(cache));
  end
  cache(key) = y;
end
%=============================================================================
function app = synthShowSignal(app, y, notes)
  root = min(notes);
  f0 = 440 * 2 ^ ((root - 69) / 12);
  k = min(numel(app.keys), max(1, root - app.lowMidi + 1));
  app.noteLabel.Text = synthNoteName(root);
  app.noteLabel.FontColor = app.keyNeon(k, :);
  app.freqLabel.Text = sprintf('%.2f Hz   |   MIDI %d', f0, root);
  if numel(notes) > 1
    app.chordLabel.Text = strjoin(arrayfun(@synthNoteName, notes, 'UniformOutput', false), '  ');
  else
    app.chordLabel.Text = synthPresetLabel(app, app.preset.Value);
  end
  mono = y(:, 1) + y(:, 2);
  start = min(numel(mono) - 1, round(0.04 * app.fs));
  cross = find(mono(start:end - 1) <= 0 & mono(start + 1:end) > 0, 1);
  if ~isempty(cross)
    start = start + cross - 1;
  end
  span = max(256, round(3 * app.fs / f0));
  segment = mono(start:min(numel(mono), start + span - 1));
  segment = segment / max(1e-9, max(abs(segment)));
  scope = interp1(linspace(0, 1, numel(segment)), segment, linspace(0, 1, 600));
  set(app.scopeLine, 'YData', scope);
  set(app.scopeGlow, 'YData', scope);
  app.spectrum = max(app.spectrum, synthBands(mono, start, app.fs));
  app.peaks = max(app.peaks, app.spectrum);
end
%=============================================================================
function bands = synthBands(mono, start, fs)
  % 64 logarithmic bands, 60 dB of range, from a Hann-windowed 4096-point FFT.
  n = 4096;
  segment = zeros(n, 1);
  available = mono(start:min(numel(mono), start + n - 1));
  segment(1:numel(available)) = available;
  window = 0.5 - 0.5 * cos(2 * pi * (0:n - 1)' / (n - 1));
  magnitude = abs(fft(segment .* window));
  magnitude = magnitude(1:n / 2);
  freqs = (0:n / 2 - 1)' * fs / n;
  edges = logspace(log10(40), log10(12000), 65);
  bands = zeros(1, 64);
  for b = 1:64
    inBand = freqs >= edges(b) & freqs < edges(b + 1);
    if any(inBand)
      bands(b) = max(magnitude(inBand));
    else
      bands(b) = interp1(freqs, magnitude, sqrt(edges(b) * edges(b + 1)));
    end
  end
  dB = 20 * log10(bands / max(1e-12, max(bands)) + 1e-12);
  bands = max(0, 1 + dB / 60);
end
%=============================================================================
function synthTick(timerObject, fig)
  % The heartbeat: sequencer, fading keys, falling bars, VU and voice LEDs.
  if ~isgraphics(fig)
    stop(timerObject);
    return
  end
  synthSequencerStep(fig);
  app = fig.UserData;
  lit = find(app.glow > 0);
  for k = lit
    g = app.glow(k) * 0.80;
    if g < 0.04
      g = 0;
    end
    app.glow(k) = g;
    app.keys{k}.FaceColor = app.keyBase(k, :) * (1 - g) + app.keyNeon(k, :) * g;
  end
  if any(app.spectrum > 0.005) || any(app.peaks > 0.005)
    app.spectrum = app.spectrum * 0.86;
    app.peaks = max(app.spectrum, app.peaks - 0.015);
    [~, by] = synthBars(app.spectrum);
    set(app.specBars, 'YData', by);
    set(app.specPeaks, 'YData', app.peaks);
  end
  alive = cellfun(@isplaying, app.players);
  cellfun(@delete, app.players(~alive));
  app.players = app.players(alive);
  voices = numel(app.players);
  colors = [repmat([0.20 0.90 0.45], 5, 1); repmat([1 0.75 0.2], 2, 1); 1 0.25 0.3];
  for k = 1:8
    wanted = app.theme.ledOff;
    if k <= voices
      wanted = colors(k, :);
    end
    if ~isequal(app.lamps{k}.Color, wanted)
      app.lamps{k}.Color = wanted;
    end
  end
  if app.level > 0.002
    app.level = app.level * 0.82;
    app.gauge.Value = min(100, 100 * app.level);
  elseif app.gauge.Value ~= 0
    app.gauge.Value = 0;
  end
  fig.UserData = app;
end
%=============================================================================
function synthSequencerStep(fig)
  % Play every keystroke of the score whose time has come.
  app = fig.UserData;
  if ~app.seq.active
    return
  end
  elapsed = toc(app.clock) - app.seq.t0;
  groups = app.seq.groups;
  while app.seq.index <= numel(groups) && groups(app.seq.index).start <= elapsed
    group = groups(app.seq.index);
    app.seq.index = app.seq.index + 1;
    fig.UserData = app;
    synthPlayNotes(fig, group.notes, group.hold);
    app = fig.UserData;
  end
  if app.seq.index > numel(groups)
    app.seq.active = false;
    fig.UserData = app;
    synthSetStatus(fig, sprintf(_('%s - finished'), app.seq.name));
  end
end
%=============================================================================
function synthStartScore(fig, score, unit, name)
  % Every keystroke is rendered before the first one sounds, so the timing
  % does not depend on how fast the machine synthesises during the piece.
  synthSetStatus(fig, sprintf(_('Preparing: %s'), name));
  drawnow();
  app = fig.UserData;
  groups = synth_piano_score_groups(score, unit);
  if app.renderCache.Count + numel(groups) > app.cacheLimit
    remove(app.renderCache, keys(app.renderCache));
  end
  for g = 1:numel(groups)
    synthRender(app, groups(g).notes, synthParams(app, groups(g).hold));
  end
  app.seq = struct('active', true, 'groups', groups, 'index', 1, ...
    't0', toc(app.clock) + 0.05, 'name', name);
  fig.UserData = app;
  synthSetStatus(fig, sprintf(_('Playing: %s'), name));
end
%=============================================================================
function synthStopScore(fig, message)
  app = fig.UserData;
  app.seq.active = false;
  fig.UserData = app;
  synthSetStatus(fig, message);
end
%=============================================================================
function synthSetStatus(fig, message)
  app = fig.UserData;
  app.status.Text = message;
end
%=============================================================================
function score = synthIntroScore()
  % A rainbow glissando over the white keys, landing on a wide C major chord.
  whites = 48 + [0 2 4 5 7 9 11];
  whites = [whites, whites + 12, whites + 24, 84];
  n = numel(whites);
  score = [(0:n - 1)', 2 * ones(n, 1), whites'];
  score = [score; repmat([n + 2, 40], 5, 1), [48 60 64 67 72]'];
end
%=============================================================================
function synthPlaySong(fig)
  app = fig.UserData;
  song = app.songs(strcmp({app.songs.name}, app.song.Value));
  app.preset.Value = song.preset;
  synthApplyPreset(fig, song.preset);
  synthStartScore(fig, song.score, song.unit, song.label);
end
%=============================================================================
function synthPresetChanged(fig)
  app = fig.UserData;
  synthApplyPreset(fig, app.preset.Value);
  synthPlayNotes(fig, [60 64 67 72], 0.6);
end
%=============================================================================
function synthApplyPreset(fig, name)
  app = fig.UserData;
  preset = app.presets(strcmp({app.presets.name}, name));
  for k = 1:4
    app.envKnobs{k}.Value = preset.knobs(k);
  end
  app.toneKnobs{1}.Value = preset.knobs(5);
  app.length.Value = preset.knobs(6);
  synthDrawEnvelope(fig);
  synthSetStatus(fig, sprintf(_('Preset: %s'), preset.label));
end
%=============================================================================
function label = synthPresetLabel(app, name)
  label = app.presets(strcmp({app.presets.name}, name)).label;
end
%=============================================================================
function synthDrawEnvelope(fig, knobIndex, knobValue)
  % Redrawn while a knob is being turned, from the very function the engine
  % applies to the sound.
  app = fig.UserData;
  env = cellfun(@(k) k.Value, app.envKnobs);
  if nargin == 3
    env(knobIndex) = knobValue;
  end
  params = synthParams(app, app.length.Value, env);
  total = params.attack + params.hold + params.release;
  t = linspace(0, total, 240)';
  env = synth_piano_envelope(t, params);
  x = t' / total;
  set(app.adsrLine, 'XData', x, 'YData', env');
  set(app.adsrGlow, 'XData', x, 'YData', env');
  set(app.adsrFill, 'XData', [x, 1, 0], 'YData', [env', 0, 0]);
end
%=============================================================================
function synthKeyPressed(fig, evt)
  app = fig.UserData;
  key = lower(char(evt.Key));
  ch = lower(char(evt.Character));
  switch key
    case {'left', 'leftarrow'}
      app.octave.Value = max(1, app.octave.Value - 1);
      synthRelabelKeys(fig);
      return
    case {'right', 'rightarrow'}
      app.octave.Value = min(6, app.octave.Value + 1);
      synthRelabelKeys(fig);
      return
    case {'up', 'uparrow', 'down', 'downarrow'}
      synthCyclePreset(fig, 1 - 2 * any(strcmp(key, {'down', 'downarrow'})));
      return
    case 'space'
      synthToggleSong(fig);
      return
    case 'shift'
      synthSetSustain(fig, true);
      return
  end
  if isempty(ch) || double(ch(1)) > 255
    return
  end
  [chars, offsets] = synthKeyMap(app.layout.Value);
  hit = find(chars == ch(1), 1);
  if isempty(hit)
    return
  end
  % A held key auto-repeats its press until released: play the first only.
  if any(strcmp(app.heldKeys, key))
    return
  end
  app.heldKeys{end + 1} = key;
  fig.UserData = app;
  synthUserNotes(fig, 12 * (app.octave.Value + 1) + offsets(hit));
end
%=============================================================================
function synthKeyReleased(fig, evt)
  key = lower(char(evt.Key));
  if strcmp(key, 'shift')
    synthSetSustain(fig, false);
    return
  end
  app = fig.UserData;
  app.heldKeys(strcmp(app.heldKeys, key)) = [];
  fig.UserData = app;
end
%=============================================================================
function synthSetSustain(fig, down)
  app = fig.UserData;
  if app.sustain == down
    return
  end
  app.sustain = down;
  if down
    app.sustainLamp.Color = app.theme.amber;
  else
    app.sustainLamp.Color = app.theme.ledOff;
  end
  fig.UserData = app;
end
%=============================================================================
function synthCyclePreset(fig, step)
  app = fig.UserData;
  names = {app.presets.name};
  k = find(strcmp(names, app.preset.Value));
  k = mod(k - 1 + step, numel(names)) + 1;
  app.preset.Value = names{k};
  synthPresetChanged(fig);
end
%=============================================================================
function synthToggleSong(fig)
  app = fig.UserData;
  if app.seq.active
    synthStopScore(fig, _('Stopped'));
  else
    synthPlaySong(fig);
  end
end
%=============================================================================
function synthRecordToggled(fig)
  app = fig.UserData;
  app.recording = logical(app.recButton.Value);
  if app.recording
    app.take = zeros(0, 3);
    app.recT0 = toc(app.clock);
    app.recLamp.Color = [1 0.2 0.25];
    message = _('Recording - play something');
  else
    app.recLamp.Color = app.theme.ledOff;
    message = sprintf(_('Take recorded: %d notes'), size(app.take, 1));
  end
  fig.UserData = app;
  synthSetStatus(fig, message);
end
%=============================================================================
function synthReplayTake(fig)
  app = fig.UserData;
  if isempty(app.take)
    synthSetStatus(fig, _('No take yet: press REC and play'));
    return
  end
  [~, order] = sort(app.take(:, 1));
  synthStartScore(fig, app.take(order, :), 1, _('Your take'));
end
%=============================================================================
function synthExport(fig)
  % Render the take (or the selected song) offline and save it as a WAV file.
  app = fig.UserData;
  if isempty(app.take)
    song = app.songs(strcmp({app.songs.name}, app.song.Value));
    score = song.score;
    unit = song.unit;
  else
    score = sortrows(app.take, 1);
    unit = 1;
  end
  synthSetStatus(fig, _('Rendering...'));
  drawnow();
  y = synthRenderScore(app, score, unit);
  filename = fullfile(tempdir(), 'nelson_ns37.wav');
  audiowrite(filename, y, app.fs);
  synthSetStatus(fig, sprintf(_('Saved %s (%.1f s)'), filename, size(y, 1) / app.fs));
end
%=============================================================================
function y = synthRenderScore(app, score, unit)
  y = zeros(0, 2);
  groups = synth_piano_score_groups(score, unit);
  for g = 1:numel(groups)
    part = synthRender(app, groups(g).notes, synthParams(app, groups(g).hold));
    first = round(groups(g).start * app.fs) + 1;
    last = first + size(part, 1) - 1;
    if last > size(y, 1)
      y(last, 2) = 0;
    end
    y(first:last, :) = y(first:last, :) + part;
  end
  y = y / max(1, max(abs(y(:))) / 0.98);
end
%=============================================================================
function synthWarmUp(fs)
  % An output device idle for a few seconds powers down and takes about a
  % second to wake up; wake it now, silently, so the first real keystroke
  % is instant.
  for k = 1:2
    player = audioplayer(zeros(round(0.02 * fs), 2), fs);
    playblocking(player);
    delete(player);
  end
end
%=============================================================================
function synthShutdown(fig)
  app = fig.UserData;
  if ~isstruct(app) || ~isfield(app, 'timer')
    return
  end
  stop(app.timer);
  delete(app.timer);
  for k = 1:numel(app.players)
    stop(app.players{k});
    delete(app.players{k});
  end
end
%=============================================================================
