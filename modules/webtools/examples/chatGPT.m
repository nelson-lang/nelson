
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
APIKey = getenv('CHATGPT_API_KEY', '');
if isempty(APIKey)
  warning(_('Please define CHATGPT_API_KEY environment variable with your private API Key.'));
end
APIUrl ='https://api.openai.com/v1/completions';
GPTModel = "text-davinci-003";
Max_Tokens = 2048;
Temperature = 0.5;
ans_count=1;
prompt = input(_('Your question to ChatGPT? '));
para.model = GPTModel;
para.prompt = prompt;
para.temperature = Temperature;
para.max_tokens =Max_Tokens;
para.n=ans_count;
para.stop='None';
options = weboptions();
Authorization = ['Bearer',' ', APIKey];
options.HeaderFields = {'ContentType', 'application/json'; 'Authorization', Authorization };
options.RequestMethod = 'POST';
options.ContentType='json';
options.MediaType='application/json';
options.Timeout=50;
response = webwrite(APIUrl, para, options);
text = response.choices.text;

% wrap text
tsz = terminal_size();
line_width = tsz(2);
num_lines = ceil(length(text)/line_width);
input_row_padded = [text, repmat(' ', 1, line_width*num_lines - length(text))];
lines_cell = cellstr(reshape(input_row_padded, line_width, num_lines)');
wrapped_text = char(lines_cell);
disp('Answer returned by ChatGPT:')
disp(wrapped_text);