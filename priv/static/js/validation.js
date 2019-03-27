var Validation = {

    "money": function(e, min, max, msg) {
        var money = e.detail.replace(/ /g,"");
        if( !/^(0\.\d{1,2}|[1-9]{1}\d{0,}(\.\d{1,2}){0,1})$/.test(money) ) {
            showErrorMSG(e.target, msg);
            return false; }
        if(!this.min(e, min)) return false;
        if(max!='none' && parseFloat(money) > max){
            showErrorMSG(e.target, i18n("MaxAmount")+max+" "+i18n(currency));
            return false; }
        var parent = e.target.parentNode;
        if(parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG") parent.removeChild(parent.lastChild);
        return true;
    },
     "pay": function(e, min, max, deptype, msg) {
            var money = document.querySelector("input[id^='money']").value;
            var pay = e.detail.replace(/ /g,"");
            if( !/^(0\.\d{1,2}|[1-9]{1}\d{0,}(\.\d{1,2}){0,1})$/.test(pay) ) {
                showErrorMSG(e.target, msg);
                return false; }
            if(!this.min(e, min)) return false;
            var maxSum = (deptype == 'DPBN') ? max : money;
            if(money!=undefined && parseFloat(pay) > maxSum) {
                showErrorMSG(e.target, i18n("MaxPayAmount")+money+" "+i18n(currency));
                return false; }
            var parent = e.target.parentNode;

            if(parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG") parent.removeChild(parent.lastChild);
            return true;
        },

    "date": function(e) {
        var min, max, val = /^{(\d{4}),(\d{2}),(\d{2})}$/.exec(e.detail);
        if(val == null) { showErrorMSG(e.target, i18n("WrongDate")); return false; }

        min = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("min"));
        max = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("max"));
        if(min==null || max==null) { showErrorMSG(e.target, i18n("RefreshPage")); return false; }

        val = parseInt(val[1]+val[2]+val[3]);
        min = parseInt(min[1]+min[2]+min[3]);
        max = parseInt(max[1]+max[2]+max[3]);
        if(val < min) { showErrorMSG(e.target, i18n("DateMin")); return false; }
        if(val > max) { showErrorMSG(e.target, i18n("DateMax")); return false; }
        if((e.target.parentNode).lastChild.tagName == "DIV" && (e.target.parentNode).lastChild.className == "errorMSG") (e.target.parentNode).removeChild((e.target.parentNode).lastChild);
        return true;
    },

    "dateRegPay": function(e) {
    console.log("calendar validation: ");
        var min, max, val = (e.detail instanceof Date) ? e.detail : null;
        if(val == null || val.toString() == "Invalid Date") { showErrorMSG(e.target, i18n("WrongDate")); return false; }
        if(val.getDate() > 28) { showErrorMSG(e.target, i18n("DateInterval")); return false; }

        if(e.target.getAttribute("type")=="calendar") {
            var picker = pickers[e.target.id];
            if(picker) {
                if(picker._d < picker._o.minDate) { showErrorMSG(e.target, i18n("DateMin")); return false; }
                if(picker._d > picker._o.maxDate) { showErrorMSG(e.target, i18n("DateMax")); return false; }
            }else { return false; }
        }else {
            min = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("min"));
            max = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("max"));
            if(min==null || max==null) { showErrorMSG(e.target, i18n("RefreshPage")); return false; }

            min = new Date(Date.parse(e.target.getAttribute("min")));
            max = new Date(Date.parse(e.target.getAttribute("max")));
            if(val < min) { showErrorMSG(e.target, i18n("DateMin")); return false; }
            if(val > max) { showErrorMSG(e.target, i18n("DateMax")); return false; }
        }
        if((e.target.parentNode).lastChild.tagName == "DIV" && (e.target.parentNode).lastChild.className == "errorMSG") (e.target.parentNode).removeChild((e.target.parentNode).lastChild);
        return true;
    },

    "chargeMoney": function(e, min, max, curr, msg) {
        if(!this.money(e, min, max, msg)) return false;
        if(curr == 980) return true;
        var cardsRates = userCardsFrom || [];
        var cardSelect = document.querySelectorAll("select[id^='cardSelectFrom_charge']")[0];
        var cardId = cardSelect.selectedOptions[0].value;
        var money = e.detail.replace(/ /g,"");
        for (var i in cardsRates) {
            if (cardsRates[i].pan == cardId) {
                var maxAmount = maxAmtExchange/cardsRates[i].convertation_rate;
                if(parseFloat(money) > maxAmount) {
                    showErrorMSG(e.target, msg);
                    return false;
                } else {
                    return true;
                }
            }
        }
        return true;
    },

    "thPersonDate": function(e) {
        var val = /^(\d{2})\.(\d{2})\.(\d{4})$/.exec(e.target.value),
            date= (e.detail instanceof Date) ? e.detail : null,
            id = e.target.id;
        if(val) { try { val = new Date(val[3],(val[2]-1),val[1]); }
                  catch(e) {val = null;} }
        if(val == null || date == null || date.toString() == "Invalid Date" || date.toString() != val.toString()) {
            showErrorMSG(e.target, i18n("WrongDate"), id+"_er", "after");
            return false; }
        showErrorMSG(e.target, "", id+"_er", "after");
        return true;
    },

    "card": function(e, msg) {
        var elId = "no_card_warning";
        if (e.target.selectedIndex == 0 && e.target.length != 1) {
            showErrorMSG(e.target, msg || "", elId);
            return false;
        } else if (e.target.value == "nocard") {
            showErrorMSG(e.target, msg || "", elId);
            return false;
        } else {
            var divmsg = (e.target.parentNode).querySelector("#"+elId) || null;
            if(divmsg != null) (divmsg.parentNode).removeChild(divmsg);
            return true;
        }
    },

    "phone": function(e, minLength, maxLength){
        if(/^\+/.test(e.detail)) {
            maxLength++;
            minLength++;
        }
        if(this.length(e, minLength, maxLength) == true) {
            if (!/^\+{0,1}\d{1,}$/.test(e.detail)){
                showErrorMSG(e.target, i18n("PhoneMsg"));
                return false;
            } else return true;
        } else return false;
    },

    "nums": function(e, minLength, maxLength, fieldType){
        if(this.length(e, minLength, maxLength) == true){
            if (!/^\d{1,}$/.test(e.detail)){
                switch(fieldType){
                    case   'otp': msg = i18n("OtpMsg"); break;
                    case 'phone': msg = i18n("PhoneMsg"); break;
                    case 'bonus': msg = i18n("BonusMsg");
                    default: msg = i18n("WrongData");
                }
                showErrorMSG(e.target, msg);
                return false;
            }else return true;
        }else return false;
    },

    "willNums": function(e, minLength, maxLength, fieldType) {
        var addHeirRadio = document.getElementById("addHeirRadio");
        var byEl = (e.target).parentNode;
        var radio = document.getElementById(byEl.id+"Radio");
        if(addHeirRadio.checked == false || radio.checked == false) { return true; }
        return this.nums(e, minLength, maxLength, fieldType);
    },

    "willLength": function(e, minlength, maxlength) {
        var addHeirRadio = document.getElementById("addHeirRadio");
        var byEl = (e.target).parentNode;
        var radio = document.getElementById(byEl.id+"Radio");
        if(addHeirRadio.checked == false || radio.checked == false) { return true; }
        return this.length(e, minLength, maxLength);
    },

    "emptyDiv": function(e) {
        var addHeirRadio = document.getElementById("addHeirRadio");
        var el = e.target;
        if(el.innerHTML != "" || addHeirRadio.checked == true) {
            if((el.parentNode.parentNode).lastChild.tagName == "DIV" && (el.parentNode.parentNode).lastChild.className == "errorMSG") {
                (el.parentNode.parentNode).removeChild((el.parentNode.parentNode).lastChild); }
            return true; }
        showErrorMSG(el.parentNode,i18n("WrongData"));
        return false;
    },

    "length": function(e, minlength, maxlength) {
        var radio6 = document.getElementById("reason6"),
        field = e.detail,
        el = e.target,
        parent = el.parentNode;
        if (radio6 != null && radio6.checked) {
            return field.length >= 1 && field.length <= maxlength;
        }else {
            if(field.length >= minlength && field.length <= maxlength) {
                hint = parent.lastChild;
                if (parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG") parent.removeChild(hint);
                return true;
            }else {
                text = (minlength == maxlength) ? i18n("CharQuantity")+ maxlength : i18n("EnterFrom")+ minlength +i18n("EnterTo")+maxlength+i18n("EnterChars");
                showErrorMSG(el, text);
                return false;
            }
        }
    },

    "min": function(e, min) {
        var field = e.detail.replace(/ /g,"");
        if( parseFloat(field) < min){
            showErrorMSG(e.target, i18n("MinAmount")+min+" "+ i18n(currency));
            return false;
        }else return true;
    },

    "empty": function(e) {
        try { var value = e.detail.trim(); }catch(e) { var value = e.detail; }
        var id = e.target.id;
        if(value == "") { showErrorMSG(e.target, i18n("EmptyField"), id+"_er", "after"); return false;}
        showErrorMSG(e.target, "", id+"_er", "after");
        return true;
    },

    "selectDep": function(e) {
        var allCheckBox = document.querySelectorAll("table.sel_dep input[type='checkbox']");
        var mainCheckbox = document.getElementById("checkAllDeps");
        if(mainCheckbox.checked) { return true; }
        for(var i=1; i<allCheckBox.length; ++i) { if(allCheckBox[i].checked) {return true;} }
        showErrorMSG(e.target, "выберите депозит");
        return false;
    },

    "set": function(form, fieldStart, msg) {
        form = document.getElementById(form);
        switch(fieldStart) {
            case "cardReg":
                el = form.querySelector("select[id^='cardReg']").parentNode.parentNode; break;
            case "card":
                el = form.querySelector("select[id^='card']").parentNode.parentNode; break;
       //     case "date":
         //       el = form.querySelector("input[id^='calendar']"); break;
            case "name":
                el = form.querySelector("input[id^='depositName']"); break;
            case "terminationRadio":
                el = form.querySelector("input[id^='myReason']"); break;
            case "codeValue":
                this.bonusCode = "BadCode";
            default:
                el = form.querySelector("input[id^='"+ fieldStart +"']");
        }
        showErrorMSG(el, msg);
    }

};

function showErrorMSG(el, msg, elId, position) {
    elId = elId || null;
    position = position || null;
    var parent = el.parentNode;
    if(!elId) {
        if(parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG")
            parent.removeChild(parent.lastChild);
    }else {
        var divmsg = parent.querySelector("#"+elId) || null;
        if(divmsg != null) (divmsg.parentNode).removeChild(divmsg);
    }
    if(msg != "") {
        el.classList.add("error");
        var div = document.createElement('div');
        div.classList.add("errorMSG");
        if(elId) div.id = elId;
        div.innerHTML = "<p style='float:left; margin:0;width:97%;'>"+msg+"</p>";
        switch(position) {
            case "after": parent.insertBefore(div,el.nextSibling); break;
            default: parent.appendChild(div);
        }
    }
}
