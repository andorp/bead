docTypeHtml $ do
    H.head $ do
        meta ! charset "utf-8"
        H.title "Bootstrap, from Twitter"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        meta ! name "description" ! content ""
        meta ! name "author" ! content ""
        link ! href "http://netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css" ! rel "stylesheet"
        script ! type_ "text/javascript" ! src "http://cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js" $ mempty
        script ! type_ "text/javascript" ! src "http://netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min.js" $ mempty
        script ! type_ "text/javascript" ! src "bootstrap-combobox.js" $ mempty
        link ! href "bootstrap-combobox.css" ! rel "stylesheet"
    body $ do
        script ! type_ "text/javascript" $ "//\n        $(document).ready(function(){\n          $('.combobox').combobox()\n        });\n      //"
        H.div ! class_ "navbar navbar-default navbar-fixed-top" $ do
            H.style ".body{padding-top:70px}"
            H.div ! class_ "container" $ do
                H.div ! class_ "navbar-header" $ do
                    a ! class_ "navbar-brand" $ "BE-AD"
                    button ! type_ "button" ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
                        H.span ! class_ "sr-only" $ "Toggle navigation"
                        H.span ! class_ "icon-bar" $ mempty
                        H.span ! class_ "icon-bar" $ mempty
                        H.span ! class_ "icon-bar" $ mempty
                H.div ! class_ "collapse navbar-collapse navbar-ex1-collapse" $ do
                    ul ! class_ "nav navbar-nav navbar-right" $ do
                        li ! class_ "active" $ a ! href "#" $ "Home"
                        li $ a ! href "#" $ "Profile"
                        li $ a ! href "#" $ do
                            "Logout"
                            br
                    p ! class_ "navbar-right navbar-text" $ "Username"
        H.div ! class_ "container" $ do
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ mempty
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ H.div ! class_ "page-header" $ h1 ! draggable "true" $ "Profile"
            H.div ! class_ "row" $ do
                H.div ! class_ "col-md-6" $ H.form $ do
                    H.div ! class_ "form-group" $ do
                        H.label ! for "exampleInputEmail1" $ "Username"
                        H.span ! class_ "form-control" ! A.id "exampleInputEmail1" $ "CRS001"
                    H.div ! class_ "form-group" $ do
                        H.label ! for "exampleInputEmail1" $ "Email address"
                        H.span ! class_ "form-control" ! A.id "exampleInputEmail1" $ "mpegdj@gmail.com"
                    H.div ! class_ "form-group" $ do
                        H.label ! for "exampleInputEmail1" $ "Full name"
                        input ! class_ "form-control" ! placeholder "CRS 001" ! type_ "text" ! A.id "exampleInputEmail1"
                    H.div ! class_ "form-group" $ select ! class_ "combobox form-control" ! A.style "display:none" ! A.id "CountrySelection" $ do
                        option ! value "" ! selected "selected" $ "Select a Timezone"
                        option ! value "Europe/Budapest" $ "Europe/Budapest"
                        option ! value "Europe/Paris" $ "Europe/Paris"
                    H.div ! class_ "form-group" $ select ! class_ "combobox form-control" ! A.style "display:none" ! A.id "LanguageSelection" $ do
                        option ! value "" ! selected "selected" $ "Select a Language"
                        option ! value "HU" $ "Magyar"
                        option ! value "EN" $ "English"
                    button ! type_ "submit" ! class_ "btn btn-block btn-default" $ "Save"
                H.div ! class_ "col-md-6" $ H.form $ do
                    H.div ! class_ "form-group" $ do
                        H.label ! for "exampleInputPassword1" $ "Old Password"
                        input ! class_ "form-control" ! A.id "exampleInputPassword1" ! placeholder "Password" ! type_ "password"
                    H.div ! class_ "form-group" $ do
                        H.label ! for "exampleInputPassword1" $ "New Password"
                        input ! class_ "form-control" ! A.id "exampleInputPassword1" ! placeholder "Password" ! type_ "password"
                    H.div ! class_ "form-group" $ do
                        H.label ! for "exampleInputPassword1" $ "New Password Again"
                        input ! class_ "form-control" ! A.id "exampleInputPassword1" ! placeholder "Password" ! type_ "password"
                    button ! type_ "submit" ! class_ "btn btn-block btn-default" $ "Update"
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr

