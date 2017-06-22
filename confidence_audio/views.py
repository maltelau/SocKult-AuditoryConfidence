from ._builtin import Page, WaitPage
from . import models
from .models import Constants
from django.conf import settings


"""
views.py defines pages, the order of the pages, and the variables needed in the templates.
Every page Page has an associated template Page.html in the templates/confidence_audio/ folder.
Those html templates are django templates with access to some python magic and the data in Page.vars_for_template(), ie
{% if sent_amount == 0 %}
    You got no money!
{% else %}
    You got {{ tripled_amount }}!
{% endif %}
"""


class Introduction(Page):
    """
    The Introduction page is shown in the first round only, but its content is also shown at
    the bottom of every other page.
    """
    def is_displayed(self):
        return self.round_number == 1
        
    def vars_for_template(self):
        return {'vowels': [[v, str(v) + "60.wav"] for v in Constants.vowels],
                'showinstructions': 'in'}
        
class GetStim(WaitPage):
    body_text = "Waiting for the other participant."
    
    def after_all_players_arrive(self):
        self.group.set_stim()
        

class Stim(Page):
    """
    Present the stimulus (once) with an invisible audio field, 
    then take individual answer.
    """

    form_model = models.Player
    form_fields = ['answer']

    def vars_for_template(self):
        return {'audiofile': self.group.stimulus}

        
class StimWaitPage(WaitPage):
    body_text = "Waiting for the other participant."
    
    def after_all_players_arrive(self):
        self.group.set_correct()

        
class Discuss(Page):
    """
    The two players didn't agree on their answer.
    Now they discuss (in chat) and reach a consensus decision
    """

    form_model = models.Group

    def get_form_fields(self):
        if self.player.control_consensus():
            return ['consensus']
        else:
            return ['ready']

    def is_displayed(self):
        """
        Logic to show this page whenever the two players disagree
        """
        [p1, p2] = self.group.get_players()
        return p1.answer != p2.answer
        
        
class DiscussWaitPage(WaitPage):
    """
    Allow people to chat while they're waiting for the other player (ie to tell them they moved on)
    """
    
    template_name = 'confidence_audio/DiscussWaitPage.html'
    
    def after_all_players_arrive(self):
        self.group.set_consensus_correct()
        
    def vars_for_template(self):      
        if self.player.control_consensus():
            return {'body_text': "Waiting for the other participant to press ready"}
        else:
            return {'body_text': "Waiting for the other participant to input your consensus decision"}

    
class Feedback(Page):
    """
    Gives group + individual feedback
    """
    
    def vars_for_template(self):
        return {'player_correct':    Constants.correct  if self.player.correct_individual               else Constants.wrong,
                'partner_correct':   Constants.correct  if self.player.get_partner().correct_individual else Constants.wrong,
                'consensus_correct': Constants.correct  if self.group.correct_consensus                 else Constants.wrong,
                'discussion':        self.player.answer != self.player.get_partner().answer}
    
    


# This variable tells otree which pages are shown and in which order.
page_sequence = [
    Introduction,
    GetStim,
    Stim,
    StimWaitPage,
    Discuss,
    DiscussWaitPage,
    Feedback
]
