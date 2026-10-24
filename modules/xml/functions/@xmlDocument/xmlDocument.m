%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Hidden) xmlDocument
  properties (Hidden)
    Text = ''
  end

  methods
    function obj = xmlDocument(text)
      if nargin > 0
        obj.Text = text;
      end
    end

    disp(varargin)
    display(varargin)
    root = getDocumentElement(doc)
    nodes = getElementsByTagName(doc, tagName)
    txt = getTextContent(doc)
  end
end
%=============================================================================
