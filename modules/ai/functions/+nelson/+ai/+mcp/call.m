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
  if nargin >= 1 && isstruct(args) && isfield(args, 'filename')
    filename = char(args.filename);
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
  % edit() resolves the name and opens it in whichever editor the surface uses.
  try
    edit(filename);
    result.output = ['Opened ', filename, ' in the Nelson editor.'];
  catch e
    result.success = false;
    result.error = e.message;
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
