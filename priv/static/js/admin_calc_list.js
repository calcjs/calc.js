function del(id) {
    $.ajax({
        url: "/admin/calc/" + id,
        type: 'DELETE'
    }).done(function() {
        var row = document.getElementById(id + "_row");
        if (row) {
            row.parentNode.removeChild(row);
        }
    });
}