from random import sample

from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer
)




doc = """
A two player auditory discriminatory task
 and chat about which was right when they didn't get it right
Audio version.


intensity:
    no zero?

analysis:
    slope at pse? <- what ric did
    pse for k=5? .2?.5?.6?
    pse?
    d prime?

    -> slope at steepest point
    
    
    
    
Models.py defines the data structures.
"""


class Constants(BaseConstants):
    """
    Constants are accessible from everywhere in the app, and are supposed to stay the same.
    """
    name_in_url = 'confidence_audio'
    players_per_group = 2
    num_rounds = 200

    # templates included in some or all pages
    instructions_template = 'confidence_audio/Instructions.html'
    
    # stimulus levels
    vowels = ["e", "i", "o", "u"]
    intensities = [5,8,11,14,17,20,23]

    # The text + icon to show correct or wrong answers
    correct = 'correct <span class="glyphicon glyphicon-ok" aria-hidden="true"></span>'
    wrong =   'wrong <span class="glyphicon glyphicon-remove" aria-hidden="true"></span>'

    


class Subsession(BaseSubsession):
    """
    A subsession represents one round of the game.
    """
    pass


class Group(BaseGroup):
    """
    A group represents one pair of participants in one round.
    It's mostly used to send and keep track of information between the two participants.
    """
    
    consensus = models.CharField(
        doc     = """Answer chosen by consensus""",
        choices = [(x,x) for x in Constants.vowels],
        widget  = widgets.RadioSelectHorizontal())
    
    ready = models.CharField(
        doc     = """Dummy field to make the non-controlling player explicitly say hes ready.""",
        choices = [("Ready", "We reached a joint decision")],
        widget  = widgets.RadioSelectHorizontal())
    
    correct_consensus = models.BooleanField(
        doc = """Whether the consensus choice was correct.""")
    
    stimulus  = models.CharField(doc = """Stimulus filename.""")
    vowel     = models.CharField(doc = """The correct vowel""")
    intensity = models.CharField(doc = """The stimulus intensity (dB of formants)""")

    def set_correct(self):
        """
        After both players answered individually, calculate whether they were correct.
        """
        
        for p in self.get_players():
            p.correct_individual = (p.answer == self.vowel)
                
    def set_consensus_correct(self):
        """
        After players reached a consensus, calculate whether they were correct.
        """
        
        self.correct_consensus = (self.consensus == self.vowel)
    
    def set_stim(self):
        """
        For each group, for each round, get a random vowel and intensity.
        """
        
        self.vowel     = sample(Constants.vowels,      1)[0]
        self.intensity = sample(Constants.intensities, 1)[0]
        
        self.stimulus  =  str(self.vowel) + str(self.intensity) + ".wav"


class Player(BasePlayer):
    """
    Player objects represent a participant in a single round.
    """
    
    answer = models.CharField(
        doc="""Answer chosen individually.""",
        choices = [(x,x) for x in Constants.vowels],
        widget = widgets.RadioSelectHorizontal())
    
    correct_individual = models.BooleanField(
        doc = """Whether the individual answer was correct.""")
    
    def role(self):
        return {1: "A", 2: "B"}[self.id_in_group]

    def get_partner(self):
        """
        Returns the other player in the group
        """
        
        return self.get_others_in_group()[0]

    def control_consensus(self):
        """
        Player 1 controls the consensus in even numbered rounds, 
        and player 2 controls it in uneven numbered rounds.
        """
        
        return (self.round_number % 2) + 1 == self.id_in_group
        
        
        
        