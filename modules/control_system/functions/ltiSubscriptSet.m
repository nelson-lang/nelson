%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = ltiSubscriptSet(obj, name, value)
  switch class(obj)
    case 'tf'
      obj = setTfProperty(obj, name, value);
    case 'ss'
      obj = setSsProperty(obj, name, value);
    case 'zpk'
      obj = setZpkProperty(obj, name, value);
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', class(obj), name);
  end
end
%=============================================================================
function obj = setZpkProperty(obj, name, value)
  switch name
    case 'IODelay'
      obj.IODelay = value;
    case 'InputDelay'
      obj.InputDelay = value;
    case 'OutputDelay'
      obj.OutputDelay = value;
    case 'InputName'
      obj.InputName = value;
    case 'InputUnit'
      obj.InputUnit = value;
    case 'InputGroup'
      obj.InputGroup = value;
    case 'OutputName'
      obj.OutputName = value;
    case 'OutputUnit'
      obj.OutputUnit = value;
    case 'OutputGroup'
      obj.OutputGroup = value;
    case 'Notes'
      obj.Notes = value;
    case 'UserData'
      obj.UserData = value;
    case 'SamplingGrid'
      obj.SamplingGrid = value;
    case 'Internal'
      obj.Internal = value;
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', 'zpk', name);
  end
end
%=============================================================================
function obj = setTfProperty(obj, name, value)
  switch name
    case 'IODelay'
      obj.IODelay = value;
    case 'InputDelay'
      obj.InputDelay = value;
    case 'OutputDelay'
      obj.OutputDelay = value;
    case 'InputName'
      obj.InputName = value;
    case 'InputUnit'
      obj.InputUnit = value;
    case 'InputGroup'
      obj.InputGroup = value;
    case 'OutputName'
      obj.OutputName = value;
    case 'OutputUnit'
      obj.OutputUnit = value;
    case 'OutputGroup'
      obj.OutputGroup = value;
    case 'Notes'
      obj.Notes = value;
    case 'UserData'
      obj.UserData = value;
    case 'SamplingGrid'
      obj.SamplingGrid = value;
    case 'Internal'
      obj.Internal = value;
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', 'tf', name);
  end
end
%=============================================================================
function obj = setSsProperty(obj, name, value)
  switch name
    case 'A'
      obj.A = value;
    case 'B'
      obj.B = value;
    case 'C'
      obj.C = value;
    case 'D'
      obj.D = value;
    case 'E'
      obj.E = value;
    case 'Offsets'
      obj.Offsets = value;
    case 'Scaled'
      obj.Scaled = value;
    case 'StateName'
      obj.StateName = value;
    case 'StatePath'
      obj.StatePath = value;
    case 'StateUnit'
      obj.StateUnit = value;
    case 'InternalDelay'
      obj.InternalDelay = value;
    case 'InputDelay'
      obj.InputDelay = value;
    case 'OutputDelay'
      obj.OutputDelay = value;
    case 'InputName'
      obj.InputName = value;
    case 'InputUnit'
      obj.InputUnit = value;
    case 'InputGroup'
      obj.InputGroup = value;
    case 'OutputName'
      obj.OutputName = value;
    case 'OutputUnit'
      obj.OutputUnit = value;
    case 'OutputGroup'
      obj.OutputGroup = value;
    case 'Notes'
      obj.Notes = value;
    case 'UserData'
      obj.UserData = value;
    case 'SamplingGrid'
      obj.SamplingGrid = value;
    case 'Internal'
      obj.Internal = value;
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', 'ss', name);
  end
end
%=============================================================================
