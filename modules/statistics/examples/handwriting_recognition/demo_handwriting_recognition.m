%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Draw a digit (0-9) or a capital letter (A-Z) with the mouse in the left-hand
% panel; Nelson shows the recognized character in large type on the right, the
% three best candidates with their scores, and the normalized image the
% classifier actually sees. Recognition is a self-contained k-nearest-neighbour
% matcher trained on synthetic handwriting (see build_handwriting_model.m) - no
% model download and no neural-network runtime, and it updates live as you draw.
%
% The recognized character is also spoken aloud (pre-recorded clips in voice/).
% Controls: Clear (or Esc) wipes the canvas, Undo (or Backspace) drops the last
% stroke, Say repeats the spoken result and the Sound box mutes it. Teach me:
% type the correct character and press Learn to add your drawing to the
% template bank for the rest of the session, so recognition adapts to your own
% hand. Close the window to quit.
function demo_handwriting_recognition()
  thisDir = fullfile(modulepath('statistics'), 'examples', 'handwriting_recognition');
  addpath(thisDir);
  loaded = load(fullfile(thisDir, 'handwriting_model.mat'));

  % Load the spoken clips (0-9, A-Z) packed as int16 in voice_clips.mat; if the
  % file is missing the demo just stays silent.
  nLab = numel(loaded.model.labels);
  voiceY = cell(1, nLab);
  voiceFs = zeros(1, nLab);
  try
    packed = load(fullfile(thisDir, 'voice_clips.mat'));
    voiceY = packed.clips.Y;
    voiceFs = packed.clips.Fs;
  catch
  end

  fig = figure('Name', 'Handwriting recognition', 'NumberTitle', 'off', ...
    'ToolBar', 'none', 'Color', [0.08 0.09 0.12], 'Position', [100 100 1040 580]);

  axDraw = axes('Parent', fig, 'Position', [0.03 0.22 0.42 0.72]);
  axis(axDraw, [0 1 0 1]);
  set(axDraw, 'YDir', 'reverse', 'XTick', [], 'YTick', [], ...
    'Color', [0.16 0.17 0.21], 'XColor', [0.40 0.45 0.55], ...
    'YColor', [0.40 0.45 0.55], 'Box', 'on');
  hold(axDraw, 'on');
  disableDefaultInteractivity(axDraw);
  title(axDraw, 'Draw here with the mouse', 'Color', [0.80 0.85 0.95]);

  axDisp = axes('Parent', fig, 'Position', [0.49 0.52 0.29 0.44]);
  axis(axDisp, [0 1 0 1]);
  axis(axDisp, 'off');
  txtChar = text(axDisp, 0.5, 0.52, '?', 'FontSize', 150, 'FontWeight', 'bold', ...
    'Color', [0.45 0.82 1.00], 'HorizontalAlignment', 'center', ...
    'VerticalAlignment', 'middle');
  txtInfo = text(axDisp, 0.5, 0.04, 'Draw a digit or a letter', 'FontSize', 12, ...
    'Color', [0.60 0.70 0.86], 'HorizontalAlignment', 'center', ...
    'VerticalAlignment', 'middle');

  % Two previews side by side: the normalized image the classifier sees, and the
  % single closest template in the bank (with its character), so the match that
  % drives the k-nearest-neighbour vote is visible.
  cmapGray = repmat(linspace(0, 1, 64)', 1, 3);
  axPrev = axes('Parent', fig, 'Position', [0.69 0.62 0.13 0.28]);
  imPrev = imagesc(axPrev, zeros(loaded.model.C, loaded.model.C));
  axis(axPrev, 'image');
  axis(axPrev, 'off');
  set(axPrev, 'CLim', [0 1]);
  colormap(axPrev, cmapGray);
  title(axPrev, 'Seen by model', 'Color', [0.60 0.70 0.86], 'FontSize', 9);
  axProto = axes('Parent', fig, 'Position', [0.84 0.62 0.13 0.28]);
  imProto = imagesc(axProto, zeros(loaded.model.C, loaded.model.C));
  axis(axProto, 'image');
  axis(axProto, 'off');
  set(axProto, 'CLim', [0 1]);
  colormap(axProto, cmapGray);
  titleProto = title(axProto, 'Closest', 'Color', [0.60 0.70 0.86], 'FontSize', 9);

  % Top-3 candidates as horizontal score bars.
  axBars = axes('Parent', fig, 'Position', [0.49 0.22 0.47 0.24]);
  set(axBars, 'XLim', [0 1], 'YLim', [0 3], 'XTick', [], 'YTick', [], ...
    'Color', [0.11 0.12 0.15], 'XColor', [0.11 0.12 0.15], ...
    'YColor', [0.11 0.12 0.15], 'Box', 'off');
  title(axBars, 'Top 3 candidates', 'Color', [0.80 0.85 0.95]);

  uicontrol('Parent', fig, 'Style', 'pushbutton', 'String', 'Clear', ...
    'Units', 'normalized', 'Position', [0.03 0.06 0.095 0.08], ...
    'FontSize', 11, 'Callback', @hwClear);
  uicontrol('Parent', fig, 'Style', 'pushbutton', 'String', 'Undo', ...
    'Units', 'normalized', 'Position', [0.135 0.06 0.095 0.08], ...
    'FontSize', 11, 'Callback', @hwUndo);
  uicontrol('Parent', fig, 'Style', 'pushbutton', 'String', 'Say', ...
    'Units', 'normalized', 'Position', [0.24 0.06 0.095 0.08], ...
    'FontSize', 11, 'Callback', @hwSay);
  soundChk = uicontrol('Parent', fig, 'Style', 'checkbox', 'String', 'Sound', ...
    'Units', 'normalized', 'Position', [0.345 0.06 0.11 0.08], ...
    'FontSize', 11, 'Value', 1, 'BackgroundColor', [0.08 0.09 0.12], ...
    'ForegroundColor', [0.80 0.85 0.95]);

  uicontrol('Parent', fig, 'Style', 'text', 'String', 'Teach me:', ...
    'Units', 'normalized', 'Position', [0.49 0.085 0.11 0.05], ...
    'FontSize', 11, 'BackgroundColor', [0.08 0.09 0.12], ...
    'ForegroundColor', [0.80 0.85 0.95], 'HorizontalAlignment', 'left');
  editTeach = uicontrol('Parent', fig, 'Style', 'edit', 'String', '', ...
    'Units', 'normalized', 'Position', [0.60 0.08 0.07 0.065], 'FontSize', 13);
  uicontrol('Parent', fig, 'Style', 'pushbutton', 'String', 'Learn', ...
    'Units', 'normalized', 'Position', [0.68 0.08 0.16 0.065], ...
    'FontSize', 11, 'Callback', @hwTeach);
  txtTeach = uicontrol('Parent', fig, 'Style', 'text', 'String', '', ...
    'Units', 'normalized', 'Position', [0.49 0.015 0.47 0.05], ...
    'FontSize', 10, 'BackgroundColor', [0.08 0.09 0.12], ...
    'ForegroundColor', [0.55 0.78 0.60], 'HorizontalAlignment', 'left');

  state.model = loaded.model;
  state.axDraw = axDraw;
  state.txtChar = txtChar;
  state.txtInfo = txtInfo;
  state.axBars = axBars;
  state.imPrev = imPrev;
  state.axPrev = axPrev;
  state.imProto = imProto;
  state.titleProto = titleProto;
  state.voiceY = voiceY;
  state.voiceFs = voiceFs;
  state.soundChk = soundChk;
  state.lastCh = '?';
  state.lastConf = 0;
  state.lastSpoken = '';
  state.editTeach = editTeach;
  state.txtTeach = txtTeach;
  state.strokes = {};
  state.lines = [];
  state.cur = [];
  state.line = [];
  state.drawing = false;
  setappdata(fig, 'hwState', state);

  set(fig, 'WindowButtonDownFcn', @hwDown, 'WindowButtonMotionFcn', @hwMove, ...
    'WindowButtonUpFcn', @hwUp, 'WindowKeyPressFcn', @hwKey);

  % Keep the demo running and responsive until its window is closed.
  while isgraphics(fig)
    drawnow();
    pause(0.03);
  end
end
%=============================================================================
function hwDown(fig, ~)
  st = getappdata(fig, 'hwState');
  p = canvasPoint(st.axDraw);
  if p(1) < 0 || p(1) > 1 || p(2) < 0 || p(2) > 1
    return;
  end
  st.drawing = true;
  st.cur = p;
  st.line = plot(st.axDraw, p(1), p(2), '-', 'Color', [0.45 0.85 1.00], ...
    'LineWidth', 7);
  setappdata(fig, 'hwState', st);
end
%=============================================================================
function hwMove(fig, ~)
  st = getappdata(fig, 'hwState');
  if ~st.drawing
    return;
  end
  p = canvasPoint(st.axDraw);
  p = [min(1, max(0, p(1))), min(1, max(0, p(2)))];
  st.cur = [st.cur; p];
  set(st.line, 'XData', st.cur(:, 1), 'YData', st.cur(:, 2));
  setappdata(fig, 'hwState', st);
  if mod(size(st.cur, 1), 3) == 0
    hwShow(fig, [st.strokes, {st.cur}]);
  end
end
%=============================================================================
function hwUp(fig, ~)
  st = getappdata(fig, 'hwState');
  if ~st.drawing
    return;
  end
  st.drawing = false;
  if ~isempty(st.cur)
    st.strokes{end + 1} = st.cur;
    st.lines(end + 1) = st.line;
  end
  st.cur = [];
  setappdata(fig, 'hwState', st);
  hwShow(fig, st.strokes);
  maybeSpeak(fig);
end
%=============================================================================
function hwKey(fig, event)
  if ~isfield(event, 'Key')
    return;
  end
  switch lower(event.Key)
    case {'escape', 'esc'}
      hwClearFig(fig);
    case {'backspace', 'back'}
      hwUndoFig(fig);
  end
end
%=============================================================================
function hwClear(button, ~)
  hwClearFig(get(button, 'Parent'));
end
%=============================================================================
function hwUndo(button, ~)
  hwUndoFig(get(button, 'Parent'));
end
%=============================================================================
function hwClearFig(fig)
  st = getappdata(fig, 'hwState');
  delete(findobj(st.axDraw, 'Type', 'line'));
  st.strokes = {};
  st.lines = [];
  st.cur = [];
  st.line = [];
  st.drawing = false;
  st.lastSpoken = '';
  st.lastCh = '?';
  st.lastConf = 0;
  setappdata(fig, 'hwState', st);
  resetDisplay(st);
end
%=============================================================================
function hwUndoFig(fig)
  st = getappdata(fig, 'hwState');
  if isempty(st.strokes)
    return;
  end
  if isgraphics(st.lines(end))
    delete(st.lines(end));
  end
  st.lines(end) = [];
  st.strokes(end) = [];
  if isempty(st.strokes)
    st.lastSpoken = '';
    st.lastCh = '?';
    st.lastConf = 0;
    setappdata(fig, 'hwState', st);
    resetDisplay(st);
  else
    setappdata(fig, 'hwState', st);
    hwShow(fig, st.strokes);
  end
end
%=============================================================================
function hwTeach(button, ~)
  fig = get(button, 'Parent');
  st = getappdata(fig, 'hwState');
  wanted = upper(strtrim(get(st.editTeach, 'String')));
  if isempty(st.strokes)
    set(st.txtTeach, 'String', 'Draw something first.', ...
      'ForegroundColor', [0.90 0.70 0.45]);
    return;
  end
  if numel(wanted) ~= 1 || ~any(st.model.labels == wanted)
    set(st.txtTeach, 'String', 'Type one character (0-9 or A-Z).', ...
      'ForegroundColor', [0.90 0.70 0.45]);
    return;
  end
  f = hw_features(hw_rasterize(st.strokes, st.model.R), st.model.C);
  st.model.X(:, end + 1) = f;
  st.model.y(end + 1) = find(st.model.labels == wanted);
  setappdata(fig, 'hwState', st);
  set(st.editTeach, 'String', '');
  set(st.txtTeach, 'String', sprintf('Learned "%c" - bank now holds %d templates.', ...
    wanted, numel(st.model.y)), 'ForegroundColor', [0.55 0.78 0.60]);
  hwShow(fig, st.strokes);
end
%=============================================================================
% Recognize strokeList and refresh every panel.
function hwShow(fig, strokeList)
  st = getappdata(fig, 'hwState');
  if isempty(strokeList)
    resetDisplay(st);
    return;
  end
  [ch, conf, topChars, topScores, featImg, protoImg, protoChar] = ...
    hw_recognize(strokeList, st.model);
  shown = ch;
  if conf < 0.40
    shown = '?';
  end
  set(st.txtChar, 'String', shown, 'Color', confColor(conf));
  set(st.txtInfo, 'String', infoLine(ch, conf, topChars, topScores), ...
    'Color', infoColor(conf));
  setPreview(st.imPrev, featImg);
  setPreview(st.imProto, protoImg);
  set(st.titleProto, 'String', sprintf('Closest: %c', protoChar));
  drawBars(st.axBars, topChars, topScores);
  st.lastCh = ch;
  st.lastConf = conf;
  setappdata(fig, 'hwState', st);
end
%=============================================================================
% Confidence line, with a "could be X or Y" hint when the top two are close.
function s = infoLine(ch, conf, topChars, topScores)
  hint = '';
  if topChars(2) ~= ' ' && topScores(2) >= 0.15 && (topScores(1) - topScores(2)) < 0.18
    hint = sprintf(' - could be %c or %c', topChars(1), topChars(2));
  end
  if conf < 0.40
    s = sprintf('uncertain - best guess %c (%.0f%%)%s', ch, conf * 100, hint);
  else
    s = sprintf('confidence %.0f%%%s', conf * 100, hint);
  end
end
%=============================================================================
function c = infoColor(conf)
  if conf < 0.40
    c = [0.90 0.70 0.45];
  else
    c = [0.60 0.70 0.86];
  end
end
%=============================================================================
function resetDisplay(st)
  set(st.txtChar, 'String', '?', 'Color', [0.45 0.82 1.00]);
  set(st.txtInfo, 'String', 'Draw a digit or a letter', 'Color', [0.60 0.70 0.86]);
  set(st.imPrev, 'CData', zeros(st.model.C, st.model.C));
  set(st.imProto, 'CData', zeros(st.model.C, st.model.C));
  set(st.titleProto, 'String', 'Closest');
  delete(findobj(st.axBars, 'Tag', 'hwbar'));
end
%=============================================================================
function setPreview(imHandle, img)
  peak = max(img(:));
  if peak > 0
    img = img / peak;
  end
  set(imHandle, 'CData', img);
end
%=============================================================================
% Blend amber (low confidence) to green (high confidence).
function c = confColor(conf)
  low = [0.95 0.65 0.35];
  high = [0.45 0.90 0.70];
  t = max(0, min(1, conf));
  c = (1 - t) * low + t * high;
end
%=============================================================================
function drawBars(axBars, topChars, topScores)
  delete(findobj(axBars, 'Tag', 'hwbar'));
  colors = [0.45 0.85 1.00; 0.50 0.60 0.78; 0.42 0.48 0.60];
  x0 = 0.12;
  maxW = 0.74;
  for i = 1:3
    if topChars(i) == ' '
      continue;
    end
    yb = 3 - i + 0.15;
    w = max(0.0, min(1, topScores(i))) * maxW;
    rectangle('Parent', axBars, 'Position', [x0, yb, max(w, 0.001), 0.52], ...
      'FaceColor', colors(i, :), 'EdgeColor', 'none', 'Tag', 'hwbar');
    text(axBars, 0.02, yb + 0.26, sprintf('%c', topChars(i)), ...
      'Color', [0.92 0.94 0.98], 'FontSize', 15, 'FontWeight', 'bold', ...
      'HorizontalAlignment', 'left', 'VerticalAlignment', 'middle', 'Tag', 'hwbar');
    text(axBars, x0 + w + 0.015, yb + 0.26, sprintf('%.0f%%', topScores(i) * 100), ...
      'Color', [0.78 0.83 0.92], 'FontSize', 10, 'VerticalAlignment', 'middle', ...
      'Tag', 'hwbar');
  end
end
%=============================================================================
function hwSay(button, ~)
  st = getappdata(get(button, 'Parent'), 'hwState');
  if st.lastCh ~= '?' && st.lastCh ~= ' '
    playChar(st, st.lastCh);
  end
end
%=============================================================================
% Speak the committed guess once, if sound is on and the result is confident
% and different from what was last spoken (so adding strokes does not repeat).
function maybeSpeak(fig)
  st = getappdata(fig, 'hwState');
  if get(st.soundChk, 'Value') ~= 1
    return;
  end
  ch = st.lastCh;
  if st.lastConf < 0.50 || ch == '?' || ch == ' ' || isequal(ch, st.lastSpoken)
    return;
  end
  playChar(st, ch);
  st.lastSpoken = ch;
  setappdata(fig, 'hwState', st);
end
%=============================================================================
function playChar(st, ch)
  idx = find(st.model.labels == ch);
  if isempty(idx) || idx > numel(st.voiceY) || isempty(st.voiceY{idx})
    return;
  end
  try
    sound(double(st.voiceY{idx}) / 32767, st.voiceFs(idx));
  catch
  end
end
%=============================================================================
function p = canvasPoint(axDraw)
  cp = get(axDraw, 'CurrentPoint');
  p = [cp(1, 1), cp(1, 2)];
end
%=============================================================================
