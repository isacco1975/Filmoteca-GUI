      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice � l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-capo � l'ID del control ef-capo
           when 78-ID-ef-capo
                inquire ef-capo, value in ef-capo-buf

           |78-ID-ef-data � l'ID del control ef-data
           when 78-ID-ef-data
                inquire ef-data, value in ef-data-buf

           end-evaluate.

