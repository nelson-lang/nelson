%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Run an external program and stream its standard input and output.
classdef Process < handle
  properties (Dependent)
    Command
    Arguments
    WorkingDirectory
    ProcessId
    Running
    ExitCode
    BytesAvailable
  end

  properties (Hidden, Access = private)
    NativeHandle
  end

  methods
    function obj = Process(command, varargin)
      narginchk(1, inf);
      obj.NativeHandle = __process__('open', command, varargin{:});
    end

    function delete(obj)
      if ~isempty(obj.NativeHandle)
        __process__('delete', obj.NativeHandle);
      end
    end

    function write(obj, data)
      __process__('write', obj.NativeHandle, data);
    end

    function out = read(obj)
      out = __process__('read', obj.NativeHandle);
    end

    function out = readline(obj)
      out = __process__('readline', obj.NativeHandle);
    end

    function out = stderr(obj)
      out = __process__('stderr', obj.NativeHandle);
    end

    function close(obj)
      __process__('close', obj.NativeHandle);
    end

    function wait(obj, timeoutSeconds)
      if nargin < 2
        timeoutSeconds = -1;
      end
      __process__('wait', obj.NativeHandle, timeoutSeconds);
    end

    function terminate(obj)
      __process__('terminate', obj.NativeHandle);
    end

    function value = get.Command(obj)
      value = __process__('get', obj.NativeHandle, 'Command');
    end

    function value = get.Arguments(obj)
      value = __process__('get', obj.NativeHandle, 'Arguments');
    end

    function value = get.WorkingDirectory(obj)
      value = __process__('get', obj.NativeHandle, 'WorkingDirectory');
    end

    function value = get.ProcessId(obj)
      value = __process__('get', obj.NativeHandle, 'ProcessId');
    end

    function value = get.Running(obj)
      value = __process__('get', obj.NativeHandle, 'Running');
    end

    function value = get.ExitCode(obj)
      value = __process__('get', obj.NativeHandle, 'ExitCode');
    end

    function value = get.BytesAvailable(obj)
      value = __process__('get', obj.NativeHandle, 'BytesAvailable');
    end
  end
end
%=============================================================================
