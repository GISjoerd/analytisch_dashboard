$(document).ready(function() {
  
/// Instellen kleur fieldlabbuttons ///  
  
 // Default buttons selecteren
  $("#a_alle_fieldlabs, #m_alle_fieldlabs").addClass("selected");
  
  //Kleur geselecteerde fieldlabbutton instellen
  $(".custom-action-button").click(function() {
    let id = $(this).attr("id");

    // Alles deselecteren
    $(".custom-action-button").removeClass("selected");

    // Juiste fiedllabbutton selecteren
    if (id === "a_alle_fieldlabs" || id === "m_alle_fieldlabs") {
      $("#a_alle_fieldlabs, #m_alle_fieldlabs").addClass("selected");
    } else if (id === "a_havelte" || id === "m_havelte") {
      $("#a_havelte, #m_havelte").addClass("selected");
    } else if (id === "a_rotterdam" || id === "m_rotterdam") {
      $("#a_rotterdam, #m_rotterdam").addClass("selected");
    } else if (id === "a_schoorl" || id === "m_schoorl") {
      $("#a_schoorl, #m_schoorl").addClass("selected");
    } else if (id === "a_sprundel" || id === "m_sprundel") {
      $("#a_sprundel, #m_sprundel").addClass("selected");
    } else if (id === "a_wamel" || id === "m_wamel") {
      $("#a_wamel, #m_wamel").addClass("selected");
    }
  });
  
/// Syncen fieldlabbuttons monitoring en analyse pagina ///
  
  let preventClickLoop = false;
  
  // Klikgebeurtenissen voor m_alle_fieldlabs en a_alle_fieldlabs

  $(document).on('click', '#m_alle_fieldlabs', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#a_alle_fieldlabs').click();
      preventClickLoop = false;
    }
  });
  
  $(document).on('click', '#a_alle_fieldlabs', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#m_alle_fieldlabs').click();
      preventClickLoop = false;
    }
  });
  
  // Klikgebeurtenissen voor m_havelte en a_havelte
  $(document).on('click', '#m_havelte', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#a_havelte').click();
      preventClickLoop = false;
    }
  });
  
  $(document).on('click', '#a_havelte', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#m_havelte').click();
      preventClickLoop = false;
    }
  });
  
  // Klikgebeurtenissen voor m_rotterdam en a_rotterdam
  $(document).on('click', '#m_rotterdam', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#a_rotterdam').click();
      preventClickLoop = false;
    }
  });
  
  $(document).on('click', '#a_rotterdam', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#m_rotterdam').click();
      preventClickLoop = false;
    }
  });
  
  // Klikgebeurtenissen voor m_schoorl en a_schoorl
  $(document).on('click', '#m_schoorl', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#a_schoorl').click();
      preventClickLoop = false;
    }
  });
  
  $(document).on('click', '#a_schoorl', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#m_schoorl').click();
      preventClickLoop = false;
    }
  });
  
  // Klikgebeurtenissen voor m_sprundel en a_sprundel
  $(document).on('click', '#m_sprundel', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#a_sprundel').click();
      preventClickLoop = false;
    }
  });
  
  $(document).on('click', '#a_sprundel', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#m_sprundel').click();
      preventClickLoop = false;
    }
  });
  
  // Klikgebeurtenissen voor m_wamel en a_wamel
  $(document).on('click', '#m_wamel', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#a_wamel').click();
      preventClickLoop = false;
    }
  });
  
  $(document).on('click', '#a_wamel', function() {
    if (!preventClickLoop) {
      preventClickLoop = true;
      $('#m_wamel').click();
      preventClickLoop = false;
    }
  });
  
/// Staafgrafiek gemiddelden actionbuttons kleur instellen ///
  $("#grafiek_hoogtes").addClass("selected");
  
  $(".action-button-graph-g").click(function() {
    $(".action-button-graph-g").removeClass("selected");
    $(this).addClass("selected");
  });
  
/// Staafgrafiek omstandigheden actionbuttons kleur instellen ///
  $("#grafiek_optimaal").addClass("selected");
  
  $(".action-button-graph-o").click(function() {
    $(".action-button-graph-o").removeClass("selected");
    $(this).addClass("selected");
  });
});


 // JavaScript functie om een PDF-bestand te downloaden
function downloadPDF(url) {
    // Maak een verborgen link element
    var link = document.createElement('a');
    link.style.display = 'none';
    document.body.appendChild(link);

    // Stel de href van de link in op het URL van de PDF
    link.href = url;

    // Stel de download attribuut in om het bestand te downloaden
    link.setAttribute('download', '');

    // Simuleer klikken op de link om het downloaden te starten
    link.click();

    // Verwijder het link element
    document.body.removeChild(link);
    console.log('Function called!');
}