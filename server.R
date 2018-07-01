shinyServer(function(input, output, session) {
    
    spotify_access_token <- reactive({
        get_spotify_access_token()
    })
    
    artist_info <- reactive({
        req(input$artist_search != '')
        get_artists(input$artist_search, access_token = spotify_access_token())
    })
    
    observeEvent(input$artist_search, {
        choices <- artist_info()$artist_name
        names(choices) <- choices
        update_material_dropdown(session, 'select_artist', value = artist_info()$artist_name[1], choices = choices)
    })
    
    output$select_artist_ui <- renderUI({
        req(nrow(artist_info()) > 0)
        tagList(
            htmlOutput('artist_img'),
            withBusyIndicatorUI(
                actionButton('tracks_go', 'Generate plot', class = 'btn-primary')
            ),
            material_switch('artist_autoplay', 'Play song preview on hover', color = '#1ed760'),
            br(),
            uiOutput('artist_chart_song_ui')
        )
    })
    
    observeEvent(input$select_artist, {
        
        req(nrow(artist_info()) > 0)
        
        artist_img <- ifelse(!is.na(artist_info()$artist_img[artist_info()$artist_name == input$select_artist]), artist_info()$artist_img[artist_info()$artist_name == input$select_artist], 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
        
        output$artist_img <- renderText({
            HTML(str_glue('<img src={artist_img} height="200">'))
        })
        
    })
    
    artist_audio_features <- eventReactive(input$tracks_go, {
        df <- get_artist_audio_features(artist_info()$artist_uri[artist_info()$artist_name == input$select_artist], access_token = spotify_access_token(), parallelize = TRUE)
        if (nrow(df) == 0) {
            stop("Sorry, couldn't find any tracks for that artist's albums on Spotify.")
        }
        return(df)
    })
    
    output$artist_quadrant_chart <- renderHighchart({
        artist_quadrant_chart(artist_audio_features()) %>% 
            hc_add_event_point(event = 'mouseOver')
    })
    
    output$artist_chart_song_ui <- renderUI({
        active_tab <- side_nav_tabs_info(input) %>% 
            filter(active) %>% 
            pull(id)
        
        req(active_tab == 'artist_tab', input$artist_quadrant_chart_mouseOver, input$artist_autoplay == TRUE)
        
        artist_track_hover <- input$artist_quadrant_chart_mouseOver
        track_preview_url <- artist_audio_features() %>% filter(
            album_name == artist_track_hover$series,
            valence == artist_track_hover$x,
            energy == artist_track_hover$y
        ) %>% pull(track_preview_url)
        
        if (!is.na(track_preview_url)) {
            tagList(
                tags$audio(id = 'song_preview', src = track_preview_url, type = 'audio/mp3', autoplay = NA, controls = NA),
                tags$script(JS("myAudio=document.getElementById('song_preview'); myAudio.play();"))
            )
        } else {
            h5('No preview for this track on Spotify')
        }
    })
    
    observeEvent(input$tracks_go, {
        output$artist_plot <- renderUI({
            if (input$GetScreenWidth >= 800) {
                withSpinner(highchartOutput('artist_quadrant_chart', width = '820px', height = '800px'), type = 5, color = '#1ed760')
            } else {
                withSpinner(highchartOutput('artist_quadrant_chart'), type = 5, color = '#1ed760')
            }
        })
    })
    
    playlists <- reactive({
        get_user_playlists(input$uri, access_token = spotify_access_token())
    })
    
    observeEvent(input$user_go, {
        withBusyIndicatorServer('user_go', {
            
            if (nchar(input$uri) == 0) {
                stop('Type a user name or Spotify URI')
            }
            
            user <- tolower(str_replace(input$uri, 'spotify:user:', ''))
            user_info <- GET(paste0(base_url, 'users/', user), query = list(access_token = spotify_access_token())) %>% content
            
            if (is.null(user_info$error)) {
                user_img <- ifelse(length(user_info$images) > 0, user_info$images[[1]]$url, 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
                
                output$user <- renderText({
                    HTML(str_glue('<img src={user_img} height="200"><br/>{user_info$display_name}'))
                })
                
                if (nrow(playlists()) == 0) {
                    stop("Sorry, that user doesn't have any playlists on Spotify.")
                }
                
                output$select_playlist_ui <- renderUI({
                    tagList(
                        withBusyIndicatorUI(
                            actionButton('playlist_go', 'Find playlists', class = 'btn-primary')
                        ),
                        material_switch('playlist_autoplay', 'Play song preview on hover', color = '#1ed760'),
                        br(),
                        uiOutput('playlist_chart_song_ui')
                    )
                })
                
            } else {
                stop("Sorry, couldn't find that user on Spotify.")
            }
        })
    })
    
    output$uri_gif <- renderText({
        HTML('<img src="user_uri.gif">')
    })
    
    playlist_tracks <- eventReactive(input$playlist_go, {
        playlist_tracks_df <- get_playlist_tracks(playlists(), access_token = spotify_access_token())
        playlist_track_audio_features <- get_track_audio_features(playlist_tracks_df, access_token = spotify_access_token())
        playlist_track_popularity <- get_track_popularity(playlist_tracks_df, access_token = spotify_access_token())
        
        track_df <- playlist_tracks_df %>%
            inner_join(playlist_track_audio_features, by = 'track_uri') %>% 
            left_join(playlist_track_popularity, by = 'track_uri')
        
        if (nrow(track_df) == 0) {
            stop("Sorry, couldn't find any tracks for that user's playlists on Spotify.")
        }
        return(track_df)
    })
    
    output$playlist_quadrant_chart <- renderHighchart({
        playlist_quadrant_chart(playlist_tracks()) %>% 
            hc_add_event_point(event = 'mouseOver')
    })
    
    output$playlist_chart_song_ui <- renderUI({
        
        active_tab <- side_nav_tabs_info(input) %>% 
            filter(active) %>% 
            pull(id)
        
        req(active_tab == 'playlist_tab', input$playlist_quadrant_chart_mouseOver, input$playlist_autoplay == TRUE)
        
        playlist_track_hover <- input$playlist_quadrant_chart_mouseOver
        track_preview_url <- playlist_tracks() %>% 
            filter(playlist_name == playlist_track_hover$series, valence == playlist_track_hover$x, energy == playlist_track_hover$y) %>% 
            pull(track_preview_url)
        
        if (!is.na(track_preview_url)) {
            tagList(
                tags$audio(id = 'song_preview', src = track_preview_url, type = 'audio/mp3', autoplay = NA, controls = NA),
                tags$script(JS("myAudio=document.getElementById('song_preview'); myAudio.play();"))
            )
        } else {
            h5('No preview for this track on Spotify')
        }
    })
    
    observeEvent(input$playlist_go, {
        output$playlist_plot <- renderUI({
            if (input$GetScreenWidth >= 800) {
                withSpinner(highchartOutput('playlist_quadrant_chart', width = '820px', height = '800px'), type = 5, color = '#1ed760')
            } else {
                withSpinner(highchartOutput('playlist_quadrant_chart'), type = 5, color = '#1ed760')
            }
        })
        
        
    })
    
})