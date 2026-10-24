%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef CompactDisplayRepresentation
  % CompactDisplayRepresentation  Result of a compact object display.
  %   Returned by the compactRepresentationForSingleLine /
  %   compactRepresentationForColumn methods of a
  %   nelson.mixin.CustomCompactDisplayProvider subclass. Usually built with
  %   the widthConstrainedDataRepresentation / fullDataRepresentation helpers.
  properties
    % PaddedDisplayOutput  char/string shown for the object.
    PaddedDisplayOutput = ''
    % Annotation  optional text appended after the data.
    Annotation = ''
    % CharacterWidth  width of the representation, in characters.
    CharacterWidth = 0
  end
  methods
    function obj = CompactDisplayRepresentation(paddedDisplayOutput, annotation)
      if nargin >= 1
        obj.PaddedDisplayOutput = paddedDisplayOutput;
      end
      if nargin >= 2
        obj.Annotation = annotation;
      end
      obj.CharacterWidth = numel(char(obj.PaddedDisplayOutput));
    end
  end
end
%=============================================================================
