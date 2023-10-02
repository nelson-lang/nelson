%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(varargin)
  TF = varargin{1};
  if nargin == 2
    name = varargin{2};
  else
    name = inputname(1);
  end
  currentFormat = format();
  if ~isempty(name)
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp([name, ' ='])
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  if isempty(TF.Numerator)
    disp(['  ', _('Empty transfer function.')])
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    return
  end
  [m, p] = size(TF.Numerator);
  numerators = TF.Numerator;
  denominator = TF.Denominator;
  
  for q = 1:p
    if (p ~= 1)
      disp(['  ',sprintf(_('From input %d to output:'), q)]);
    end
    for k = 1:m
      numeratorString = stringPoly(numerators{k, q}, TF.Variable);
      denominatorString = stringPoly(denominator{k, q}, TF.Variable);
      lengthDiff = length(denominatorString) - length(numeratorString);
      if (lengthDiff > 0)
        spacesToAdd = repmat(' ', 1, floor(lengthDiff / 2));
        numeratorString = [spacesToAdd, numeratorString];
      end
      dashString = getDashedLine(numeratorString, denominatorString);
      if (m ~= 1)
        if (strcmp(denominatorString, ' 1') == true)
          disp(['  ',sprintf(_('%d:'), k), '  ', numeratorString]);
        else
          disp(['  ',sprintf(_('%d:'), k)])
          disp(['  ', numeratorString]);
       end
      else
        disp(['  ', numeratorString]);
      end
      if (strcmp(denominatorString, ' 1') ~= true)
        disp(['  ', dashString]);
        disp(['  ', denominatorString]);
      end
      if strcmp(currentFormat.LineSpacing, 'loose')
        disp(' ');
      end
    end
  end
  if (isstatic(TF))
    disp(_('     Static gain.'));
  else
    if isdt(TF)
      disp(sprintf(_('Sample time: %.4f %s'), TF.Ts, TF.TimeUnit)); 
      disp(_('Discrete-time transfer function.'));
    end
    if isct(TF)
      disp(_('Continuous-time transfer function.'));
    end
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
end
%=============================================================================
function dash = getDashedLine(numeratorString, denominatorString)
  dash = '';
  lenDash = max(length(numeratorString), length(denominatorString)) + 1;
  for i = 1:lenDash
    dash = strcat(dash, '—');
  end
end
%=============================================================================
function returnString = stringPoly(array, variableName)
  if any(strcmp(variableName, {'z^-1', 'q^-1'}))
    returnString = stringPolyNegative(array, variableName);
  else
    returnString = stringPolyPositive(array, variableName);
  end
end
%=============================================================================
function returnString = stringPolyPositive(array, variableName)
  polyString = {}; % Begin with empty cell string
  % Get all to a string cell
  for i = 1:length(array)
    if(and(array(i) ~= 0, length(array) - i ~= 0))
      if(length(array) - i > 1)
        if(and(array(i) <= 1.000001, array(i) >= 0.999999))
          polyString(1, i) = {strcat([variableName, '^'], num2str(length(array) - i))};
        elseif(and(array(i) >= -1.000001, array(i) <= -0.999999))
          polyString(1, i) = {strcat(['- ', variableName, '^'], num2str(length(array) - i))};
        else
          polyString(1, i) = {strcat(num2str(array(i)), [' ',variableName, '^'], num2str(length(array) - i))};
        end
      else
        if(and(array(i) <= 1.000001, array(i) >= 0.999999))
          polyString(1, i) = {[' ', strcat(variableName)]};
        elseif(and(array(i) >= -1.000001, array(i) <= -0.999999))
          polyString(1, i) = {['-', ' ', variableName]};
        else
          if (array(i) < 0)
            polyString(1, i) = {['- ', num2str(abs(array(i))), ' ', variableName]};
            
          else
            polyString(1, i) = {[num2str(array(i)), ' ', variableName]};
          end
        end
      end
    else
      if (array(i) < 0)
        polyString(1, i) = {['- ' strcat(num2str(abs(array(i))))]};
      else    
        polyString(1, i) = {strcat(num2str(array(i)))};
      end
    end
  end
  
  % Transform that string cell to a complete string
  returnString = '';
  for i = 1:length(polyString)
    element = char(polyString(1, i));
    % No only-zeros are allowed
    if(and(~strcmp(element, '0'), ~strcmp(element, '-0')))
      if(length(strfind(element, '-')) > 0)
        returnString = [returnString, ' ', element];
      else
        if(length(returnString) == 0)
          returnString = [returnString, ' ', element];
        else
          returnString = [returnString,' + ', element];
        end
      end
    end
  end
end
%=============================================================================
function returnString = stringPolyNegative(array, variableName)
  if strcmp(variableName, 'z^-1')
    variableName = 'z';
  else
    variableName = 'q';
  end
  polyString = {}; % Begin with empty cell string
  % Get all to a string cell
  for i = 1:length(array)
    valueSign = sign(array(i));
    valueAbs = abs(array(i));
    if (valueSign > 0)
      if i == 0
        polyString(1, i) =  { [num2str(valueAbs)]};
      else
        if (i - 1 == 0)
          polyString(1, i) =  { [num2str(valueAbs)]};
        else
          if strcmp(num2str(valueAbs), '1')
            if (i - 1 == 0)
              polyString(1, i) =  { [' 1']};
            else
              polyString(1, i) =  { [' ', variableName, '^-', num2str(i-1)]};
            end
          else
            if (i - 1 == 0)
              polyString(1, i) =  { [' ', num2str(valueAbs)]};
            else
              polyString(1, i) =  { [' ', num2str(valueAbs), ' ', variableName, '^-', num2str(i-1)]};
            end
          end
        end
      end
    else
      if valueSign == -1
        if (i == 0)
          polyString(1, i) =  { [' - ', num2str(valueAbs)]};
        else
          if strcmp(num2str(valueAbs), '1')
            if (i-1 == 0)
              polyString(1, i) =  { [' - 1']};
            else
              polyString(1, i) =  { [' - ', variableName, '^-', num2str(i-1)]};
            end
          else
            if (i-1 == 0)
              polyString(1, i) =  { [' - ', num2str(valueAbs)]};
            else
              polyString(1, i) =  { [' - ', num2str(valueAbs), ' ', variableName, '^-', num2str(i-1)]};
            end
          end
        end
      else
        polyString(1, i) =  {''};
      end
    end
  end
  
  % Transform that string cell to a complete string
  returnString = '';
  for i = 1:length(polyString)
    element = char(polyString(1, i));
    if(and(~strcmp(element, '0'), ~strcmp(element, '-0')))
      if startsWith(element, ' -')
        returnString = [returnString,  element];
      else
        if isempty(returnString)
          returnString = [returnString, element];
        else  
          returnString = [returnString, ' +', element];
        end
      end
    end
  end
end
%=============================================================================
