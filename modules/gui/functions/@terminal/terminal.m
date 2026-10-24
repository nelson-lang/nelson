%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef terminal < handle
  properties (Dependent)
    Name
    Shell
    Place
    WindowStyle
    Running
    ExitCode
    ProcessId
    Theme
  end

  properties (Access = private)
    NativeHandle = []
  end

  methods
    function obj = terminal(varargin)
      if nargin == 2 && ischar(varargin{2}) && strcmp(varargin{2}, 'native')
        obj.NativeHandle = varargin{1};
        return
      end
      options = defaultOptions();
      if nargin > 0
        if mod(nargin, 2) ~= 0
          error('Nelson:terminal:nameValueExpected', 'Name-value arguments expected.');
        end
        for k = 1:2:nargin
          [name, msg] = resolveOptionName(varargin{k});
          if ~isempty(msg)
            error('Nelson:terminal:invalidOption', msg);
          end
          value = varargin{k + 1};
          switch name
            case 'Name'
              options.Name = validateTextOrError('Name', value);
            case 'Shell'
              options.Shell = validateTextOrError('Shell', value);
            case 'WindowStyle'
              options.WindowStyle = validateChoiceOrError('WindowStyle', value, {'docked'; 'normal'});
            case 'Theme'
              options.Theme = validateChoiceOrError('Theme', value, terminal.themes());
            case 'StartupCommand'
              options.StartupCommand = validateTextOrError('StartupCommand', value);
          end
        end
      end
      obj.NativeHandle = __terminal__('create', options);
    end

    function value = get.Name(obj)
      value = getNativeProperty(obj, 'Name');
    end

    function set.Name(obj, value)
      __terminal__('set', obj.NativeHandle, 'Name', validateTextOrError('Name', value));
    end

    function value = get.Shell(obj)
      value = getNativeProperty(obj, 'Shell');
    end

    function value = get.Place(obj)
      value = getNativeProperty(obj, 'Place');
    end

    function value = get.WindowStyle(obj)
      value = getNativeProperty(obj, 'WindowStyle');
    end

    function value = get.Running(obj)
      value = getNativeProperty(obj, 'Running');
    end

    function value = get.ExitCode(obj)
      value = getNativeProperty(obj, 'ExitCode');
    end

    function value = get.ProcessId(obj)
      value = getNativeProperty(obj, 'ProcessId');
    end

    function value = get.Theme(obj)
      value = getNativeProperty(obj, 'Theme');
    end

    function set.Theme(obj, value)
      value = validateChoiceOrError('Theme', value, terminal.themes());
      __terminal__('set', obj.NativeHandle, 'Theme', value);
    end

    function run(obj, cmd)
      obj.writeln(cmd);
    end

    function write(obj, text)
      __terminal__('write', obj.NativeHandle, validateTextOrError('Text', text));
    end

    function writeln(obj, text)
      text = validateTextOrError('Text', text);
      if ispc()
        obj.write([text, char(13), char(10)]);
      else
        obj.write([text, char(10)]);
      end
    end

    function text = read(obj)
      text = __terminal__('read', obj.NativeHandle);
    end

    function clear(obj)
      __terminal__('clear', obj.NativeHandle);
    end

    function focus(obj)
      __terminal__('focus', obj.NativeHandle);
    end

    function close(obj)
      if ~isempty(obj.NativeHandle)
        __terminal__('close', obj.NativeHandle);
        obj.NativeHandle = [];
      end
    end

    function delete(obj)
      obj.close();
    end

    function tf = isrunning(obj)
      tf = obj.Running;
    end
  end

  methods (Static)
    function value = list()
      nativeHandles = __terminal__('list');
      if isempty(nativeHandles)
        value = [];
        return
      end
      value = terminal(nativeHandles(1), 'native');
      for k = 2:numel(nativeHandles)
        value(k) = terminal(nativeHandles(k), 'native');
      end
    end

    function closeAll()
      __terminal__('closeAll');
    end

    function value = version()
      value = __terminal__('version');
    end

    function value = themes()
      value = __terminal__('themes');
    end
  end
end
%=============================================================================
function options = defaultOptions()
  options = struct();
  options.Name = '';
  options.Shell = '';
  options.WindowStyle = 'docked';
  options.Theme = 'auto';
  options.StartupCommand = '';
end
%=============================================================================
function value = getNativeProperty(obj, name)
  value = __terminal__('get', obj.NativeHandle, name);
end
%=============================================================================
function [canonical, msg] = resolveOptionName(name)
  canonical = '';
  [name, msg] = validateText('Name', name);
  if ~isempty(msg)
    return
  end
  unsupported = {'Model'; 'Agentic'; 'Agent'; 'Toolkits'; 'AgentCLI'; 'Tabs'};
  if any(strcmpi(unsupported, name))
    msg = sprintf('"%s" is not supported by terminal.', name);
    return
  end
  supported = {'Name'; 'Shell'; 'WindowStyle'; 'Theme'; 'StartupCommand'};
  matches = startsWith(upper(supported), upper(name));
  if sum(matches) == 0
    msg = sprintf(_('"%s" is not a recognized parameter.'), name);
    return
  end
  if sum(matches) > 1
    msg = sprintf(_('"%s" matches multiple parameters.'), name);
    return
  end
  canonical = supported{find(matches, 1)};
end
%=============================================================================
function value = validateTextOrError(name, value)
  [value, msg] = validateText(name, value);
  if ~isempty(msg)
    error('Nelson:terminal:invalidValue', msg);
  end
end
%=============================================================================
function [value, msg] = validateText(name, value)
  msg = '';
  if isstring(value)
    if ~isscalar(value)
      msg = sprintf(_('%s must be a character vector or string scalar.'), name);
      value = '';
      return
    end
    value = char(value);
  end
  if ~ischar(value) || size(value, 1) ~= 1
    msg = sprintf(_('%s must be a character vector or string scalar.'), name);
    value = '';
  end
end
%=============================================================================
function value = validateChoiceOrError(name, value, supported)
  [value, msg] = validateText(name, value);
  if ~isempty(msg)
    error('Nelson:terminal:invalidValue', msg);
  end
  matches = strcmpi(supported, value);
  if sum(matches) ~= 1
    error('Nelson:terminal:invalidValue', 'Invalid value for "%s".', name);
  end
  value = supported{find(matches, 1)};
end
%=============================================================================
