%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function displayPreamble(obj, isLoose)
  % The event labels variable, and the event lengths or ends variable.
  st = struct(obj);
  vars = eventtablePropertiesView(st.Properties, st.EventVariables);
  builtin('disp', ['  Event Labels Variable: ', variableText(vars.EventLabelsVariable, '<unset>')]);
  if isempty(vars.EventEndsVariable)
    builtin('disp', ['  Event Lengths Variable: ', variableText(vars.EventLengthsVariable, '<instantaneous>')]);
  else
    builtin('disp', ['  Event Ends Variable: ', vars.EventEndsVariable]);
  end
  if isLoose
    fprintf('\n');
  end
end
%=============================================================================
function text = variableText(name, defaultText)
  if isempty(name)
    text = defaultText;
  else
    text = name;
  end
end
%=============================================================================
