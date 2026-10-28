%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = subscriptGet(obj, name)
  className = class(obj);
  switch className
    case 'tf'
      value = getTfProperty(obj, name);
    case 'ss'
      value = getSsProperty(obj, name);
    case 'zpk'
      value = getZpkProperty(obj, name);
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', className, name);
  end
end
%=============================================================================
function value = getZpkProperty(obj, name)
  switch name
    case 'Z'
      value = obj.Z;
    case 'P'
      value = obj.P;
    case 'K'
      value = obj.K;
    case 'Numerator'
      value = obj.Numerator;
    case 'Denominator'
      value = obj.Denominator;
    case 'Variable'
      value = obj.Variable;
    case 'IODelay'
      value = obj.IODelay;
    case 'InputDelay'
      value = obj.InputDelay;
    case 'OutputDelay'
      value = obj.OutputDelay;
    case 'InputName'
      value = obj.InputName;
    case 'InputUnit'
      value = obj.InputUnit;
    case 'InputGroup'
      value = obj.InputGroup;
    case 'OutputName'
      value = obj.OutputName;
    case 'OutputUnit'
      value = obj.OutputUnit;
    case 'OutputGroup'
      value = obj.OutputGroup;
    case 'Notes'
      value = obj.Notes;
    case 'UserData'
      value = obj.UserData;
    case 'Name'
      value = obj.Name;
    case 'Ts'
      value = obj.Ts;
    case 'TimeUnit'
      value = obj.TimeUnit;
    case 'SamplingGrid'
      value = obj.SamplingGrid;
    case 'Internal'
      value = obj.Internal;
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', 'zpk', name);
  end
end
%=============================================================================
function value = getTfProperty(obj, name)
  switch name
    case 'Numerator'
      value = obj.Numerator;
    case 'Denominator'
      value = obj.Denominator;
    case 'Variable'
      value = obj.Variable;
    case 'IODelay'
      value = obj.IODelay;
    case 'InputDelay'
      value = obj.InputDelay;
    case 'OutputDelay'
      value = obj.OutputDelay;
    case 'InputName'
      value = obj.InputName;
    case 'InputUnit'
      value = obj.InputUnit;
    case 'InputGroup'
      value = obj.InputGroup;
    case 'OutputName'
      value = obj.OutputName;
    case 'OutputUnit'
      value = obj.OutputUnit;
    case 'OutputGroup'
      value = obj.OutputGroup;
    case 'Notes'
      value = obj.Notes;
    case 'UserData'
      value = obj.UserData;
    case 'Name'
      value = obj.Name;
    case 'Ts'
      value = obj.Ts;
    case 'TimeUnit'
      value = obj.TimeUnit;
    case 'SamplingGrid'
      value = obj.SamplingGrid;
    case 'Internal'
      value = obj.Internal;
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', 'tf', name);
  end
end
%=============================================================================
function value = getSsProperty(obj, name)
  switch name
    case 'A'
      value = obj.A;
    case 'B'
      value = obj.B;
    case 'C'
      value = obj.C;
    case 'D'
      value = obj.D;
    case 'E'
      value = obj.E;
    case 'Offsets'
      value = obj.Offsets;
    case 'Scaled'
      value = obj.Scaled;
    case 'StateName'
      value = obj.StateName;
    case 'StatePath'
      value = obj.StatePath;
    case 'StateUnit'
      value = obj.StateUnit;
    case 'InternalDelay'
      value = obj.InternalDelay;
    case 'InputDelay'
      value = obj.InputDelay;
    case 'OutputDelay'
      value = obj.OutputDelay;
    case 'InputName'
      value = obj.InputName;
    case 'InputUnit'
      value = obj.InputUnit;
    case 'InputGroup'
      value = obj.InputGroup;
    case 'OutputName'
      value = obj.OutputName;
    case 'OutputUnit'
      value = obj.OutputUnit;
    case 'OutputGroup'
      value = obj.OutputGroup;
    case 'Notes'
      value = obj.Notes;
    case 'UserData'
      value = obj.UserData;
    case 'Name'
      value = obj.Name;
    case 'Ts'
      value = obj.Ts;
    case 'TimeUnit'
      value = obj.TimeUnit;
    case 'SamplingGrid'
      value = obj.SamplingGrid;
    case 'Internal'
      value = obj.Internal;
    otherwise
      error('Nelson:control_system:unknownProperty', 'No property of the class ''%s'' matches the identifier ''%s''.', 'ss', name);
  end
end
%=============================================================================
