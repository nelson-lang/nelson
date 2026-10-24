%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef htmlviewer < handle
  properties (Dependent)
    Input
    Visible
  end

  properties (Access = private)
    ViewerID = 0
    InputValue = ''
    VisibleValue = true
    IsNative = true
    IsClosed = false
  end

  methods
    function obj = htmlviewer(varargin)
      if nargin == 3 && ischar(varargin{3}) && strcmp(varargin{3}, 'native')
        obj.ViewerID = varargin{1};
        obj.InputValue = varargin{2};
        obj.VisibleValue = true;
        obj.IsNative = true;
        obj.IsClosed = false;
        return
      end
      if nargin == 3 && ischar(varargin{3}) && strcmp(varargin{3}, 'fallback')
        obj.ViewerID = 0;
        obj.InputValue = varargin{2};
        obj.VisibleValue = true;
        obj.IsNative = false;
        obj.IsClosed = false;
        return
      end
      if nargin > 1
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      input = '';
      if nargin == 1
        input = nelson.htmlviewer.htmlviewer.validateTextScalar(varargin{1});
      end
      [viewerId, currentInput] = __webview__('open', input, true, true, true);
      obj.ViewerID = viewerId;
      obj.InputValue = currentInput;
      obj.VisibleValue = true;
    end

    function value = get.Input(obj)
      value = obj.InputValue;
    end

    function set.Input(obj, value)
      obj.ensureOpen();
      value = nelson.htmlviewer.htmlviewer.validateTextScalar(value);
      if obj.IsNative
        obj.InputValue = __webview__('setInput', obj.ViewerID, value);
      else
        obj.InputValue = value;
      end
    end

    function value = get.Visible(obj)
      value = obj.VisibleValue;
    end

    function set.Visible(obj, value)
      obj.ensureOpen();
      value = nelson.htmlviewer.htmlviewer.validateLogicalScalar(value);
      if obj.IsNative
        __webview__('setVisible', obj.ViewerID, value);
      end
      obj.VisibleValue = value;
    end

    function close(obj)
      if obj.IsNative && obj.ViewerID ~= 0
        __webview__('close', obj.ViewerID);
      end
      obj.ViewerID = 0;
      obj.IsClosed = true;
      obj.VisibleValue = false;
    end

    function delete(obj)
      obj.close();
    end

    function htmlText = getHTMLText(obj)
      obj.ensureOpen();
      if obj.IsNative
        htmlText = __webview__('getHtmlText', obj.ViewerID);
      else
        htmlText = nelson.htmlviewer.htmlviewer.htmlTextFromInput(obj.InputValue, obj.VisibleValue);
      end
    end
  end

  methods (Hidden)
    function value = getBackend(obj)
      % Which backend is showing this viewer: 'webview' for a native window, a
      % browser name when the native path failed and web() fell back to one,
      % 'fallback' when there is no WebUI window at all. Diagnostic only: those
      % three outcomes are otherwise indistinguishable from the caller.
      if ~obj.IsNative
        value = 'fallback';
        return
      end
      obj.ensureOpen();
      value = __webview__('getBackend', obj.ViewerID);
    end
  end

  methods (Access = private)
    function ensureOpen(obj)
      if obj.IsClosed
        error('Nelson:htmlviewer:closed', 'HTML viewer is closed.');
      end
      if obj.IsNative
        if obj.ViewerID == 0 || ~__webview__('isOpen', obj.ViewerID)
          error('Nelson:htmlviewer:closed', 'HTML viewer is closed.');
        end
      end
    end
  end

  methods (Static, Hidden)
    function obj = create(input, newWindow, showToolbar, showAddressBox)
      input = nelson.htmlviewer.htmlviewer.validateTextScalar(input);
      [viewerId, currentInput] = __webview__('open', input, ...
        nelson.htmlviewer.htmlviewer.validateLogicalScalar(newWindow), ...
        nelson.htmlviewer.htmlviewer.validateLogicalScalar(showToolbar), ...
        nelson.htmlviewer.htmlviewer.validateLogicalScalar(showAddressBox));
      obj = nelson.htmlviewer.htmlviewer(viewerId, currentInput, 'native');
    end

    function obj = createFallback(input)
      input = nelson.htmlviewer.htmlviewer.validateTextScalar(input);
      obj = nelson.htmlviewer.htmlviewer(0, input, 'fallback');
    end

    function value = validateTextScalar(value)
      supported = (ischar(value) && size(value, 1) == 1) || (isstring(value) && isscalar(value));
      if ~supported
        error('Nelson:htmlviewer:textScalarExpected', 'Text scalar expected.');
      end
      value = convertStringsToChars(value);
    end

    function value = validateLogicalScalar(value)
      if islogical(value) && isscalar(value)
        value = logical(value);
        return
      end
      if isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
        value = logical(value);
        return
      end
      error('Nelson:webview:logicalScalarExpected', 'Logical scalar expected.');
    end

    function htmlText = htmlTextFromInput(input, visible)
      if ~visible
        htmlText = nelson.htmlviewer.htmlviewer.blankHtml();
        return
      end
      if isempty(input)
        htmlText = nelson.htmlviewer.htmlviewer.blankHtml();
        return
      end
      lowerInput = lower(input);
      if startsWith(lowerInput, 'text://')
        if length(input) > 7
          htmlText = input(8:end);
        else
          htmlText = '';
        end
        return
      end
      if startsWith(lowerInput, 'file://')
        if length(input) > 7
          filename = input(8:end);
        else
          filename = '';
        end
        if ispc() && length(filename) >= 3 && strcmp(filename(1), '/') && strcmp(filename(3), ':')
          filename = filename(2:end);
        end
        if isfile(filename)
          htmlText = fileread(filename);
          return
        end
      end
      if isfile(input)
        htmlText = fileread(input);
      else
        htmlText = input;
      end
    end

    function htmlText = blankHtml()
      htmlText = '<!doctype html><html><head><meta charset="utf-8"></head><body></body></html>';
    end
  end
end
%=============================================================================
