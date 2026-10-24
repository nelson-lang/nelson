%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Hidden) xmlElement
  properties (Hidden)
    Text = ''
    Name = ''
  end

  methods
    function obj = xmlElement(text, name)
      if nargin > 0
        obj.Text = text;
      end
      if nargin > 1
        obj.Name = name;
      end
    end

    disp(varargin)
    display(varargin)
    value = getAttribute(element, name)
    nodes = getElementsByTagName(element, tagName)
    name = getNodeName(element)
    name = getTagName(element)
    txt = getTextContent(element)
    tf = hasAttribute(element, name)
  end
end
%=============================================================================
