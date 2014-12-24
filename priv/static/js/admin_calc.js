function addCalculationRow() {
    var calculationsTable = $('#calcTable')[0];
    if (calculationsTable) {
        var index = calculationsTable.rows.length - 1;
        var row = calculationsTable.insertRow(index);
        var currentFormId = $('#id').val();
        row.id = "row_" + index;
        row.innerHTML = '<td><input type="text" size="34" name="form_id_' + index + '" id="form_id_' + index + '" value="' + currentFormId + '" /></td><td><input type="text" size="3" name="order_' + index + '" id="order_' + index + '" value="' + index + '" /></td><td><input type="text" size="14" name="field_' + index + '" id="field_' + index + '" value="" /></td><td><input type="checkbox" name="repeating_' + index + '"></td><td><textarea type="text" name="formula_' + index + '" id="formula_' + index + '" rows="4" cols="100" ></textarea></td><td><input type="checkbox" name="always_execute_' + index + '"></td><td><input type="checkbox" name="label_' + index + '"></td><td><input type="checkbox" name="active_' + index + '" checked></td><td><input type="button" onClick="deleteCalculationRow('+ index + ')" value="Delete" /></td>';
        $('#numCalculations')[0].value = index;
    }
}

function addDependentForm() {
    var dependentFormTable = $('#depFormTable')[0];
    if (dependentFormTable) {
        var index = dependentFormTable.rows.length - 1;
        var row = dependentFormTable.insertRow(index);
        row.innerHTML = '<td><input type="text" size="34" name="dependent_form_id_' + index + '" value=""></td><td><input type="text"" name="dep_form_data_retrieval_api_' + index + '" /></td><td><input type="text"" name="dep_form_post_api_' + index + '" /></td><td><input type="text"" name="dep_form_field_info_api_' + index + '" /></td><td><input type="button" onClick="deleteDependentFormRow(' + index + ')" value="Delete" %}" /></td>';
        $('#numDependentForms')[0].value = index;
    }
}

function deleteCalculationRow(index) {
    var calculationsTable = $('#calcTable')[0];
    if (calculationsTable) {
        var row = document.getElementById("row_" + index);
        if (row) {
            row.parentNode.removeChild(row);
            updateInputFieldIndex('calcTable', index);
            updateDeleteButtonIndex('calcTable', index);
        
            $('#numCalculations')[0].value = calculationsTable.rows.length - 2;
        }
    }
}

function deleteDependentFormRow(index) {
    var dependentFormTable = $('#depFormTable')[0];
    if (dependentFormTable) {
        dependentFormTable.deleteRow(index);
        
        updateInputFieldIndex('depFormTable', index);
        updateDeleteButtonIndex('depFormTable', index);
        
        $('#numDependentForms')[0].value = dependentFormTable.rows.length - 2;
    }
}

function updateInputFieldIndex(tableId, index) {
    var inputFields = $('#' + tableId + ' :input');
    
    for (var i=0; i<inputFields.length; i++) {
        if (inputFields[i].name) {
            var fieldNameParts = inputFields[i].name.split('_');
            var previousNumber = fieldNameParts[fieldNameParts.length - 1];
            if (previousNumber > index) {
                var lastIndex = inputFields[i].name.lastIndexOf('_');
                inputFields[i].name = inputFields[i].name.substring(0, lastIndex) + "_" + (previousNumber - 1);
            }
        }
    }
}

function updateDeleteButtonIndex(tableId, index) {
    var inputFields = $('#' + tableId + ' :input');
    
    for (var i=0; i<inputFields.length; i++) {
        if (inputFields[i].type && inputFields[i].onclick && inputFields[i].value && inputFields[i].value == '削除') {
            var onClickValue = inputFields[i].getAttribute("onclick");
            
            var onClickValueParts = onClickValue.split('(');
            var onClickValueEndParts = onClickValueParts[1].split(')');
            
            var previousNumber = onClickValueEndParts[0];
            if (previousNumber > index) {
                var attributeValue = onClickValueParts[0] + "(" + (previousNumber - 1) + ")";
                inputFields[i].setAttribute("onclick", attributeValue);
            }
        }
    }
}
