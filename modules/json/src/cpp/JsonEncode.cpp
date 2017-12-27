//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include "JsonEncode.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson
{
	//=============================================================================
	static std::string jsonString = "";
	//=============================================================================
	static void json_append_char(char c)
	{
		jsonString.push_back(c);
	}
	//=============================================================================
	static void json_append_string(std::string str)
	{
		jsonString = jsonString + str;
	}
	//=============================================================================
	static bool isSupportedType(ArrayOf ValueToEncode)
	{
		if (ValueToEncode.isClassStruct()) return false;
		if (ValueToEncode.isSparse()) return false;
		switch (ValueToEncode.getDataClass())
		{
		case NLS_HANDLE:
			return false;
		case NLS_CELL_ARRAY:
		case NLS_STRUCT_ARRAY:
		case NLS_LOGICAL:
		case NLS_UINT8:
		case NLS_INT8:
		case NLS_UINT16:
		case NLS_INT16:
		case NLS_UINT32:
		case NLS_INT32:
		case NLS_UINT64:
		case NLS_INT64:
		case NLS_SINGLE:
		case NLS_DOUBLE:
		case NLS_CHAR:
			return true;
		case NLS_SCOMPLEX:
			return false;
		case NLS_DCOMPLEX:
			return false;
		default:
			return false;
		}
		return false;
	}
	//=============================================================================
	static ArrayOf jsonEncodeInternal(ArrayOf ValueToEncode, std::wstring &errorMessage)
	{
		if (!isSupportedType(ValueToEncode))
		{
			errorMessage = L"Unsupported type to convert as JSON.";
			return ArrayOf();
		}
		if (ValueToEncode.isEmpty())
		{
			json_append_string("[]");
		}
		else if (ValueToEncode.isSingleString())
		{
			json_append_char('"');
			std::string str = ValueToEncode.getContentAsCString();
			for (size_t i = 0; i < str.size(); i++) 
			{
				switch (str[i])
				{
				case '"':
				case '\\':
				case '/':
					json_append_char('\\');
					json_append_char(str[i]);
					break;
				case '\b':
					json_append_string("\\b");
					break;
				case '\f':
					json_append_string("\\f");
					break;
				case '\n':
					json_append_string("\\n");
					break;
				case '\r':
					json_append_string("\\r");
					break;
				case '\t':
					json_append_string("\\t");
					break;
				default:
				{
					if ((str[i] < 32) || (str[i] > 126))
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "\\u%04hx", str[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
					else
					{
						json_append_char(str[i]);
					}

				}
				break;
				}
			}
			json_append_char('"');
		}
		else
		{
			indexType nbElements = ValueToEncode.getDimensions().getElementCount();
			if (nbElements > 1) json_append_char('[');
			switch (ValueToEncode.getDataClass())
			{
			case NLS_DCOMPLEX:
			case NLS_SCOMPLEX:
			case NLS_HANDLE:
			{
				errorMessage = L"Unsupported type to convert as JSON.";
				return ArrayOf();
			}
			break;
			case NLS_CELL_ARRAY:
			{
				ArrayOf *elements = (ArrayOf *)ValueToEncode.getDataPointer();
				for (int i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
				{
					jsonEncodeInternal(elements[i], errorMessage);
					json_append_char(',');
				}
			}
			break;
			case NLS_STRUCT_ARRAY:
			{
				stringVector fieldnames = ValueToEncode.getFieldNames();
				for (int i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
				{
					json_append_char('{');
					for (int field = 0; field < fieldnames.size(); field++)
					{
						std::string fieldname = fieldnames[field];
						ArrayOfVector values = ValueToEncode.getFieldAsList(fieldname);
						if (!values.empty())
						{
							json_append_string("\"" + fieldname + "\":");
							jsonEncodeInternal(values[i], errorMessage);
							json_append_char(',');
						}
					}
					if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
					json_append_char('}');
					json_append_char(',');
				}
			}
			break;
			case NLS_LOGICAL:
			{
				logical *ptr = (logical *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						if (ptr[i] == 0)
						{
							json_append_string("false,");
						}
						else
						{
							json_append_string("true,");
						}
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							if (ptr[i] == 0)
							{
								json_append_string("false,");
							}
							else
							{
								json_append_string("true,");
							}
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							if (ptr[i] == 0)
							{
								json_append_string("false,");
							}
							else
							{
								json_append_string("true,");
							}
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_UINT8:
			{
				uint8 *ptr = (uint8 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%u,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%u,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%u,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_INT8:
			{
				int8 *ptr = (int8 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%i,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%i,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%i,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_UINT16:
			{
				uint16 *ptr = (uint16 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%u,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%u,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%u,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_INT16:
			{
				int16 *ptr = (int16 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%i,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%i,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%i,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_UINT32:
			{
				uint32 *ptr = (uint32 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%u,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%u,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%u,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_INT32:
			{
				int32 *ptr = (int32 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%i,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%i,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%i,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_UINT64:
			{
				uint64 *ptr = (uint64 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%llu,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%llu,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%llu,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_INT64:
			{
				int64 *ptr = (int64 *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%lli,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%lli,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%lli,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_SINGLE:
			{
				single *ptr = (single *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%.16g,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%.16g,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%.16g,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_DOUBLE:
			{
				double *ptr = (double *)ValueToEncode.getDataPointer();
				if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector())
				{
					for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++)
					{
						char buff[1024];
						snprintf(buff, sizeof(buff), "%.16g,", ptr[i]);
						std::string buffAsStdStr = buff;
						json_append_string(buffAsStdStr);
					}
				}
				else if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < cols; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%.16g,", ptr[j*rows + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('[');
						for (int j = 0; j < lastdimlen; ++j)
						{
							char buff[1024];
							snprintf(buff, sizeof(buff), "%.16g,", ptr[j*ymax + i]);
							std::string buffAsStdStr = buff;
							json_append_string(buffAsStdStr);
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("],");
					}
				}
			}
			break;
			case NLS_CHAR:
			{
				std::wstring strw = ValueToEncode.getContentAsArrayOfCharacters();
				if (ValueToEncode.is2D())
				{
					int rows = ValueToEncode.getDimensions().getRows();
					int cols = ValueToEncode.getDimensions().getColumns();
					for (int i = 0; i < rows; ++i)
					{
						json_append_char('\"');
						for (int j = 0; j < cols; ++j)
						{
							switch (strw[j*rows + i])
							{
							case L'"':
							case L'\\':
							case L'/':
							{
								json_append_char('\\');
								std::wstring wstr;
								wstr.push_back(strw[j*rows + i]);
								std::string str = wstring_to_utf8(wstr);
								json_append_string(str);
							}
							break;
							case L'\b':
								json_append_string("\\b");
								break;
							case L'\f':
								json_append_string("\\f");
								break;
							case L'\n':
								json_append_string("\\n");
								break;
							case L'\r':
								json_append_string("\\r");
								break;
							case L'\t':
								json_append_string("\\t");
								break;
							default:
							{
								if ((strw[j*rows + i] < 32) || (strw[j*rows + i] > 126))
								{
									char buff[1024];
									snprintf(buff, sizeof(buff), "\\u%04hx", strw[j*rows + i]);
									std::string buffAsStdStr = buff;
									json_append_string(buffAsStdStr);
								}
								else
								{
									std::wstring wstr;
									wstr.push_back(strw[j*rows + i]);
									std::string str = wstring_to_utf8(wstr);
									json_append_string(str);
								}
							}
							break;
							}
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("\",");
					}
				}
				else
				{
					Dimensions dims = ValueToEncode.getDimensions();
					int lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
					int ymax = dims.getElementCount() / lastdimlen;
					for (int i = 0; i < ymax; ++i)
					{
						json_append_char('\"');
						for (int j = 0; j < lastdimlen; ++j)
						{
							wchar_t ch = strw[i*ymax + j];
							switch (ch)
							{
							case L'"':
							case L'\\':
							case L'/':
							{
								json_append_char('\\');
								std::wstring wstr;
								wstr.push_back(ch);
								std::string str = wstring_to_utf8(wstr);
								json_append_string(str);
							}
							break;
							case L'\b':
								json_append_string("\\b");
								break;
							case L'\f':
								json_append_string("\\f");
								break;
							case L'\n':
								json_append_string("\\n");
								break;
							case L'\r':
								json_append_string("\\r");
								break;
							case L'\t':
								json_append_string("\\t");
								break;
							default:
							{
								if ((ch < 32) || (ch > 126))
								{
									char buff[1024];
									snprintf(buff, sizeof(buff), "\\u%04hx,", ch);
									std::string buffAsStdStr = buff;
									json_append_string(buffAsStdStr);
								}
								else
								{
									std::wstring wstr;
									wstr.push_back(strw[j*ymax + i]);
									std::string str = wstring_to_utf8(wstr);
									json_append_string(str);
								}
							}
							break;
							}
						}
						if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
						json_append_string("\",");
					}
				}
			}
			break;
			default:
			{
				errorMessage = L"Unsupported type to convert as JSON.";
				return ArrayOf();
			}
			break;
			}
			if (boost::algorithm::ends_with(jsonString, L",")) jsonString.pop_back();
			if (nbElements > 1) json_append_char(']');
		}
		return ArrayOf::stringConstructor(jsonString);
	}
	//=============================================================================
	ArrayOf jsonEncode(ArrayOf ValueToEncode, std::wstring &errorMessage)
	{
		jsonString.clear();
		ArrayOf res = jsonEncodeInternal(ValueToEncode, errorMessage);
		jsonString.clear();
		return res;
	}
	//=============================================================================
}
//=============================================================================
