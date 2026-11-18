%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Invoke a Nelson assistant tool in the current session.
function result = call(name, varargin)
  % Pane/host-side actions run on the live interpreter here (they drive a GUI
  % widget or push to the live pane) rather than going through the native tool
  % registry. insert_* push code into an open surface; open_nelson_file /
  % launch_nflow / open_nelson_notebook let the assistant pilot the widgets.
  switch name
    case 'insert_into_nelson_editor'
      result = insertIntoEditor(varargin{:});
      return
    case 'insert_into_nelson_notebook'
      result = insertIntoNotebook(varargin{:});
      return
    case 'insert_into_nelson_file'
      result = insertIntoFile(varargin{:});
      return
    case 'open_nelson_file'
      result = openNelsonFile(varargin{:});
      return
    case 'launch_nflow'
      result = launchNflow(varargin{:});
      return
    case 'open_nelson_notebook'
      result = openNelsonNotebook(varargin{:});
      return
    case 'open_nelson_doc'
      result = openNelsonDoc(varargin{:});
      return
    case 'run_current_file'
      result = runCurrentFile(varargin{:});
      return
    case 'profile_current_file'
      result = profileCurrentFile(varargin{:});
      return
    case 'debug_nelson_file'
      result = debugNelsonFile(varargin{:});
      return
    case 'clear_breakpoints'
      result = clearBreakpoints(varargin{:});
      return
    case 'propose_nelson_edit'
      result = proposeNelsonEdit(varargin{:});
      return
  end
  result = __mcp_call__(name, varargin{:});
end
%=============================================================================
function result = insertIntoEditor(args)
  result = struct('success', true, 'output', 'Inserted code at the editor cursor.', ...
    'error', '', 'image', '', 'data', '', 'duration', 0);
  code = '';
  if nargin >= 1 && isstruct(args) && isfield(args, 'code')
    code = args.code;
  end
  if isempty(code)
    result.success = false;
    result.output = '';
    result.error = 'code is required.';
    return
  end
  __ai_notify__('editorInsert', jsonencode(struct('text', char(code))));
end
%=============================================================================
function result = insertIntoFile(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  filename = '';
  code = '';
  if nargin >= 1 && isstruct(args)
    if isfield(args, 'filename')
      filename = char(args.filename);
    end
    if isfield(args, 'code')
      code = char(args.code);
    end
  end
  if isempty(filename)
    result.success = false;
    result.error = 'filename is required.';
    return
  end
  if isempty(code)
    result.success = false;
    result.error = 'code is required.';
    return
  end
  % Open the file in the editor and drop the code into its buffer; the user
  % reviews and saves. editorInsert carries the filename so the pane opens that
  % file first, then inserts (one atomic step, so the code lands in the right
  % file). Nothing is written to disk without the user saving.
  __ai_notify__('editorInsert', jsonencode(struct('filename', filename, 'text', char(code))));
  result.output = ['Inserted code into ', filename, ' (review and save in the editor).'];
end
%=============================================================================
function result = insertIntoNotebook(args)
  result = struct('success', true, 'output', 'Added a code cell to the notebook.', ...
    'error', '', 'image', '', 'data', '', 'duration', 0);
  code = '';
  run = false;
  if nargin >= 1 && isstruct(args)
    if isfield(args, 'code')
      code = args.code;
    end
    if isfield(args, 'run')
      run = logical(args.run);
    end
  end
  if isempty(code)
    result.success = false;
    result.output = '';
    result.error = 'code is required.';
    return
  end
  if run
    result.output = 'Added a code cell to the notebook and ran it.';
  end
  __ai_notify__('notebookInsert', jsonencode(struct('text', char(code), 'run', run)));
end
%=============================================================================
function result = openNelsonFile(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  filename = '';
  line = 0;
  if nargin >= 1 && isstruct(args)
    if isfield(args, 'filename')
      filename = char(args.filename);
    end
    % Optional: jump to a line (e.g. the one named in an error or a search hit).
    if isfield(args, 'line') && ~isempty(args.line) && isnumeric(args.line)
      line = double(args.line);
    end
  end
  if isempty(filename)
    result.success = false;
    result.error = 'filename is required.';
    return
  end
  % Only open a file that exists on disk: edit() of a missing name pops a native
  % "create it?" dialog, which the assistant must never trigger (e.g. a draft that
  % was inserted but not yet saved). Report it instead so the agent tells the user.
  if ~isfile(filename)
    result.success = false;
    result.error = ['The file ', filename, ' does not exist on disk. ', ...
      'Save it first, or pass the full path to an existing file.'];
    return
  end
  % editor() resolves the name and opens it in whichever editor the surface uses
  % (Qt, web desktop, external); with a line it scrolls there.
  try
    if line >= 1
      editor(filename, line);
      result.output = ['Opened ', filename, ' at line ', int2str(line), ...
        ' in the Nelson editor.'];
    else
      edit(filename);
      result.output = ['Opened ', filename, ' in the Nelson editor.'];
    end
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
function result = openNelsonDoc(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  topic = '';
  if nargin >= 1 && isstruct(args) && isfield(args, 'topic')
    topic = char(args.topic);
  end
  if isempty(topic)
    result.success = false;
    result.error = 'topic is required.';
    return
  end
  % doc() opens the help browser on the topic (a function name or keyword), on
  % whichever surface the session uses.
  try
    doc(topic);
    result.output = ['Opened the documentation for ', topic, '.'];
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
function result = runCurrentFile(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  % Run a SAVED Nelson file in the console. With an explicit "filename" it runs that
  % file even when NO editor is open (the common case right after the user saves a
  % proposed or inserted file); without one it runs the file currently open in the
  % editor, as clicking Run would. Only a file that exists on disk is run - never
  % code supplied by the marker, never an unsaved draft - so this is not a back door
  % for arbitrary execution, it just removes the editor-open requirement.
  [filename, err] = targetSavedFile(args, 'run');
  if isempty(filename)
    result.success = false;
    result.error = err;
    return
  end
  try
    run(filename);
    result.output = ['Ran ', filename, ' in the console.'];
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
% Resolve the target SAVED file for a run/profile/debug action: the explicit
% "filename" argument, or the file currently open in the editor. Returns '' and a
% clear, verb-specific error when none is given or it is not on disk, so one bad
% input never runs stale content. Shared by run/profile/debug so they behave alike.
function [filename, err] = targetSavedFile(args, verb)
  filename = '';
  err = '';
  fromEditor = false;
  if nargin >= 1 && isstruct(args) && isfield(args, 'filename') && ~isempty(args.filename)
    filename = char(args.filename);
  else
    fromEditor = true;
    try
      filename = char(__editor__('currentFile'));
    catch
      filename = '';
    end
  end
  if isempty(filename)
    filename = '';
    err = ['No file to ', verb, ': no "filename" was given and no file is open in ', ...
      'the editor. Pass {"filename": "..."} with a saved file.'];
    return
  end
  if ~isfile(filename)
    if fromEditor
      err = ['No saved file is open in the editor to ', verb, '. Save it first, or ', ...
        'pass {"filename": "..."} with a saved file.'];
    else
      err = ['The file ', filename, ' does not exist on disk. Save it first, or pass ', ...
        'the full path to an existing saved file.'];
    end
    filename = '';
  end
end
%=============================================================================
% Profile a SAVED Nelson file in the console and summarize its hot spots. The run
% is bracketed by profile('on')/profile('off') (after a clear so the report is just
% this run), then profile('info') is sorted by total time for a top-N text summary.
% The full interactive report is the live Profiler panel, which shows the same data
% on both the Qt and the web surface. Only an on-disk saved file is run (same bound
% as run_current_file), so it needs no editor open and is not a code backdoor.
function result = profileCurrentFile(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  [filename, err] = targetSavedFile(args, 'profile');
  if isempty(filename)
    result.success = false;
    result.error = err;
    return
  end
  runError = '';
  try
    profile('clear');
    profile('on');
    try
      run(filename);
    catch e
      runError = e.message;
    end
    profile('off');
  catch e
    result.success = false;
    result.error = ['Profiler error: ', e.message];
    return
  end
  info = dropFramesFromFile(profile('info'), [mfilename('fullpath'), '.m']);
  summary = profileSummary(info, 8);
  notifyShowPanel('profiler');
  head = ['Profiled ', filename, ' in the console. Top functions by total time:', ...
    char(10), summary, char(10), 'Opening the Profiler panel for the full ', ...
    'interactive report.'];
  if isempty(runError)
    result.output = head;
  else
    result.output = [head, char(10), 'Note: the file raised an error while profiling: ', ...
      runError];
  end
end
%=============================================================================
% Ask the host to bring a GUI panel to the front (same AssistantSink channel the
% editor actions use, so it reaches both the Qt dock and the web panel). Best-effort:
% a no-op without a pane, so it never fails the action.
function notifyShowPanel(panel)
  try
    __ai_notify__('showPanel', jsonencode(struct('panel', char(panel))));
  catch
  end
end
%=============================================================================
% Drop the profiler frames that belong to the host action itself (this call.m), so
% the summary shows the user's code, not the run() wrapper used to launch it.
function info = dropFramesFromFile(info, selfPath)
  if ~isstruct(info) || isempty(info) || ~isfield(info, 'Filename')
    return
  end
  self = normalizePath(selfPath);
  keep = true(1, numel(info));
  for k = 1:numel(info)
    if strcmp(normalizePath(char(info(k).Filename)), self)
      keep(k) = false;
    end
  end
  info = info(keep);
end
%=============================================================================
function p = normalizePath(p)
  p = lower(strrep(char(p), '\', '/'));
end
%=============================================================================
% A top-N "function (file:line) - calls, time" summary of a profile('info') struct
% array, sorted by total time. '' guarded so an empty profile never breaks the reply.
function text = profileSummary(info, maxLines)
  text = '(no profile data was collected)';
  if ~isstruct(info) || isempty(info) || ~isfield(info, 'TotalTime')
    return
  end
  times = double([info.TotalTime]);
  [~, order] = sort(times, 'descend');
  info = info(order);
  n = min(numel(info), maxLines);
  lines = cell(1, n);
  for k = 1:n
    e = info(k);
    [~, base, ext] = fileparts(char(e.Filename));
    lines{k} = ['  ', char(e.FunctionName), ' (', base, ext, ':', ...
      int2str(double(e.LinePosition)), ') - ', int2str(double(e.NumCalls)), ...
      ' calls, ', num2str(double(e.TotalTime), '%.4g'), 's'];
  end
  text = strjoin(lines, char(10));
end
%=============================================================================
% Set a breakpoint in a SAVED Nelson file and open it at that line, so the live
% debugger GUI (breakpoint gutter, Call Stack and variables, on both the Qt and the
% web surface) engages when the user next runs the code. With no "line" it opens the
% file and lists its current breakpoints instead. The assistant sets debugging up;
% the interactive stepping stays the user's in the GUI.
function result = debugNelsonFile(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  [filename, err] = targetSavedFile(args, 'debug');
  if isempty(filename)
    result.success = false;
    result.error = err;
    return
  end
  line = 0;
  if nargin >= 1 && isstruct(args) && isfield(args, 'line') && ~isempty(args.line) ...
      && isnumeric(args.line)
    line = double(args.line);
  end
  try
    if line >= 1
      dbstop('in', filename, 'at', int2str(line));
      opened = openInEditor(filename, line);
      runHint = ' Run the code (for example run_current_file); execution pauses there in the debugger, showing the Call Stack and the variables.';
      if opened
        result.output = ['Set a breakpoint at line ', int2str(line), ' in ', filename, ...
          ' and opened it there.', runHint];
      else
        result.output = ['Set a breakpoint at line ', int2str(line), ' in ', filename, ...
          '. Open the file and', runHint];
      end
    else
      openInEditor(filename, 0);
      result.output = ['Targeted ', filename, ' for debugging. ', ...
        breakpointStatus(filename), ' Pass {"line": N} to set a breakpoint at a line.'];
    end
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
% Best-effort open of a file in the editor (at a line when line >= 1). Returns
% whether it opened: the editor is a GUI surface that may be absent (headless), so a
% failure here never fails the debug action - the breakpoint is already set.
function opened = openInEditor(filename, line)
  opened = false;
  try
    if line >= 1
      editor(filename, line);
    else
      edit(filename);
    end
    opened = true;
  catch
    opened = false;
  end
end
%=============================================================================
% A short sentence on the breakpoints currently set in a file.
function text = breakpointStatus(filename)
  text = '';
  try
    status = dbstatus(filename);
  catch
    return
  end
  if isempty(status)
    text = 'No breakpoints are set in it yet.';
  else
    text = ['It has ', int2str(numel(status)), ' breakpoint entr', ...
      pluralY(numel(status)), ' set.'];
  end
end
%=============================================================================
function s = pluralY(n)
  if n == 1
    s = 'y';
  else
    s = 'ies';
  end
end
%=============================================================================
% Clear breakpoints: those of a given "filename", or all when none is given. Used
% to reset a debugging session the assistant set up.
function result = clearBreakpoints(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  filename = '';
  if nargin >= 1 && isstruct(args) && isfield(args, 'filename') && ~isempty(args.filename)
    filename = char(args.filename);
  end
  try
    if isempty(filename)
      dbclear('all');
      result.output = 'Cleared all breakpoints.';
    else
      dbclear('in', filename);
      result.output = ['Cleared the breakpoints in ', filename, '.'];
    end
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
function result = proposeNelsonEdit(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  filename = '';
  code = '';
  if nargin >= 1 && isstruct(args)
    if isfield(args, 'filename')
      filename = char(args.filename);
    end
    if isfield(args, 'code')
      code = char(args.code);
    end
  end
  if isempty(filename)
    result.success = false;
    result.error = 'filename is required.';
    return
  end
  if isempty(code)
    result.success = false;
    result.error = 'code is required.';
    return
  end
  % Propose a full-file edit: show the user what changes (a unified diff computed
  % against the current on-disk content with the shipped diff_file engine) and
  % open the proposed version in the editor in replace mode, so the user reviews
  % the whole new file and saves to apply. Nothing is written to disk until then.
  diffText = computeProposedDiff(filename, code);
  __ai_notify__('editorInsert', ...
    jsonencode(struct('filename', filename, 'text', char(code), 'replace', true)));
  if isempty(diffText)
    result.output = ['Opened the proposed version of ', filename, ...
      ' in the editor (review and save to apply).'];
  else
    result.output = ['Proposed changes to ', filename, ...
      ' (review and save in the editor to apply):', char(10), diffText];
  end
end
%=============================================================================
function diffText = computeProposedDiff(filename, code)
  diffText = '';
  if ~isfile(filename)
    % A new file has nothing to diff against; the editor shows the full content.
    return
  end
  tmp = [tempname(), '.txt'];
  fid = fopen(tmp, 'w');
  if fid < 0
    return
  end
  fprintf(fid, '%s', code);
  fclose(fid);
  try
    diffText = char(diff_file(filename, tmp));
  catch
    diffText = '';
  end
  if isfile(tmp)
    delete(tmp);
  end
end
%=============================================================================
function result = launchNflow(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  model = '';
  if nargin >= 1 && isstruct(args) && isfield(args, 'model')
    model = char(args.model);
  end
  % nflow() opens an empty editor window; nflow(model) opens the given model.
  try
    if isempty(model)
      nflow();
      result.output = 'Opened the NFlow editor.';
    else
      nflow(model);
      result.output = ['Opened the NFlow editor with ', model, '.'];
    end
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
function result = openNelsonNotebook(args)
  result = struct('success', true, 'output', '', 'error', '', 'image', '', ...
    'data', '', 'duration', 0);
  filename = '';
  if nargin >= 1 && isstruct(args) && isfield(args, 'filename')
    filename = char(args.filename);
  end
  if isempty(filename)
    result.success = false;
    result.error = 'filename is required.';
    return
  end
  try
    notebook(filename, 'edit');
    result.output = ['Opened ', filename, ' in the notebook editor.'];
  catch e
    result.success = false;
    result.error = e.message;
  end
end
%=============================================================================
